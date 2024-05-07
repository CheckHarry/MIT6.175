// Reference functions that use Bluespec's '*' operator
function Bit#(TAdd#(n,n)) multiply_unsigned( Bit#(n) a, Bit#(n) b );
    UInt#(n) a_uint = unpack(a);
    UInt#(n) b_uint = unpack(b);
    UInt#(TAdd#(n,n)) product_uint = zeroExtend(a_uint) * zeroExtend(b_uint);
    return pack( product_uint );
endfunction

function Bit#(TAdd#(n,n)) multiply_signed( Bit#(n) a, Bit#(n) b );
    Int#(n) a_int = unpack(a);
    Int#(n) b_int = unpack(b);
    Int#(TAdd#(n,n)) product_int = signExtend(a_int) * signExtend(b_int);
    return pack( product_int );
endfunction

// Multiplication by repeated addition
function Bit#(TAdd#(n,n)) multiply_by_adding(Bit#(n) a, Bit#(n) b);
    Bit#(TAdd#(n,n)) to_return = 0;

    for (Integer i = 0 ; i < valueOf(n) ; i = i + 1)
    begin
        if (b[i] == 1) 
        begin
            to_return = to_return + (zeroExtend(a) << i);
        end
    end

    return to_return;
    
endfunction

// Multiplier Interface
interface Multiplier#( numeric type n );
    method Bool start_ready();
    method Action start( Bit#(n) a, Bit#(n) b );
    method Bool result_ready();
    method ActionValue#(Bit#(TAdd#(n,n))) result();
endinterface


// Folded multiplier by repeated addition
module mkFoldedMultiplier( Multiplier#(n) )
	provisos(Add#(1, a__, n)); // make sure n >= 1

    // You can use these registers or create your own if you want
    Reg#(Bit#(n)) a <- mkRegU();
    Reg#(Bit#(n)) b <- mkRegU();
    Reg#(Bit#(TAdd#(n,n))) res <- mkRegU();
    Reg#(Bit#(n)) i <- mkReg(0);
    Reg#(Bool) srdy <- mkReg(True);

    rule mulStep if (i < fromInteger(valueOf(n)));
        if (a[i] == 1)
        begin 
            res <= res + (zeroExtend(b) << i);
        end
        i <= i + 1;
    endrule

    method Bool start_ready();
        return srdy;
    endmethod

    method Action start( Bit#(n) aIn, Bit#(n) bIn ) if (srdy);
        srdy <= False;
        a <= aIn;
        b <= bIn;
        res <= 0;
        i <= 0;
    endmethod

    method Bool result_ready() if (i == fromInteger(valueOf(n)));
        return (i == fromInteger(valueOf(n)));
    endmethod

    method ActionValue#(Bit#(TAdd#(n,n))) result if (i == fromInteger(valueOf(n)));
        srdy <= True;
        return res;
    endmethod
endmodule



function Bit#(n) arth_shift(Bit#(n) a, Integer n, Bool right_shift);
    Int#(n) a_int = unpack(a);
    if (right_shift) begin
        return  pack(a_int >> n);
    end else begin //left shift
        return  pack(a_int <<n);
    end
endfunction



// Booth Multiplier
module mkBoothMultiplier( Multiplier#(n) )
	provisos(Add#(2, a__, n)); // make sure n >= 2

    Reg#(Bit#(n)) i <- mkReg(0);
    Reg#(Bool) srdy <- mkReg(True);
    Reg#(Bit#(TAdd#(TAdd#(n , n) , 1))) m_pos <- mkRegU();
    Reg#(Bit#(TAdd#(TAdd#(n , n) , 1))) m_neg <- mkRegU();
    Reg#(Bit#(TAdd#(TAdd#(n , n) , 1))) p <- mkRegU();
    

    rule mul_step if (i < fromInteger(valueOf(n)));
        Bit#(2) pr = p[1:0];
        Int#(TAdd#(TAdd#(n , n) , 1)) pnext = unpack(p);
        Int#(TAdd#(TAdd#(n , n) , 1)) m_pos_int = unpack(m_pos);
        Int#(TAdd#(TAdd#(n , n) , 1)) m_neg_int = unpack(m_neg);
        if (pr == 2'b01)
        begin 
            p <= pack((pnext + m_pos_int) >> 1);
        end
        else if (pr == 2'b10)
        begin 
            p <= pack((pnext + m_neg_int) >> 1);
        end
        else
        begin
            p <= pack(pnext >> 1);
        end
        i <= i + 1;

    endrule

    method Bool start_ready();
        return srdy;
    endmethod

    method Action start( Bit#(n) m, Bit#(n) r ) if (srdy);
        m_pos <= {m , 0};
        m_neg <= {-m , 0};
        p <= {0 , r , 1'b0};
        i <= 0;
        srdy <= False;
    endmethod

    method Bool result_ready() ;
        return (i == fromInteger(valueOf(n)));
    endmethod

    method ActionValue#(Bit#(TAdd#(n,n))) result() if (i == fromInteger(valueOf(n)));
        srdy <= True;
        Bit#(TAdd#(n,n)) res = p[2 * valueOf(n):1];
        //$display("%b" , p);
        return res;
    endmethod
endmodule



// Radix-4 Booth Multiplier
module mkBoothMultiplierRadix4( Multiplier#(n) )
	provisos(Mul#(a__, 2, n), Add#(1, b__, a__)); // make sure n >= 2 and n is even

    Reg#(Bit#(n)) i <- mkReg(0);
    Reg#(Bool) srdy <- mkReg(True);
    Reg#(Bit#(TAdd#(TAdd#(n , n) , 2))) m_pos <- mkRegU();
    Reg#(Bit#(TAdd#(TAdd#(n , n) , 2))) m_neg <- mkRegU();
    Reg#(Bit#(TAdd#(TAdd#(n , n) , 2))) p <- mkRegU();

    rule mul_step(i < fromInteger(valueOf(n)));
        Bit#(3) pr = p[2:0];
        Int#(TAdd#(TAdd#(n , n) , 2)) pnext = unpack(p);
        Int#(TAdd#(TAdd#(n , n) , 2)) m_pos_int = unpack(m_pos);
        Int#(TAdd#(TAdd#(n , n) , 2)) m_neg_int = unpack(m_neg);
        if (pr == 3'b001 || pr == 3'b010)
        begin 
            p <= pack((pnext + m_pos_int) >> 2);
        end
        else if (pr == 3'b110 || pr == 3'b101)
        begin 
            p <= pack((pnext + m_neg_int) >> 2);
        end
        else if (pr == 3'b011)
        begin
            p <= pack((pnext + (m_pos_int << 1)) >> 2);
        end
        else if (pr == 3'b100)
        begin
            p <= pack((pnext + (m_neg_int << 1)) >> 2);
        end
        else
        begin
            p <= pack(pnext >> 2);
        end
        i <= i + 2;
    endrule

    method Bool start_ready();
        return srdy;
    endmethod

    method Action start( Bit#(n) m, Bit#(n) r ) if (srdy);
        m_pos <= {msb(m) , m , 0};
        m_neg <= {msb(-m) , -m , 0};
        p <= {0 , r , 1'b0};
        i <= 0;
        srdy <= False;
    endmethod

    method Bool result_ready();
        return (i == fromInteger(valueOf(n)));
    endmethod

    method ActionValue#(Bit#(TAdd#(n,n))) result() if (i == fromInteger(valueOf(n)));
        srdy <= True;
        return  p[2 * valueOf(n):1];
    endmethod
endmodule
