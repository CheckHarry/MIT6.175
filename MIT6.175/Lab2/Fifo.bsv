import Ehr::*;
import Vector::*;
import FIFO::*;

interface Fifo#(numeric type n, type t);
    method Action enq(t x);
    method Action deq;
    method t first;
    method Bool notEmpty;
    method Bool notFull;
endinterface

// Exercise 1
// Completes the code in Fifo.bsv to implements a 3-elements fifo with properly
// guarded methods. Feel free to take inspiration from the class slides.
// The interface defined in Fifo.bsv tells you the type of the methods
// (enq, deq, first) that your module should define.
module mkFifo(Fifo#(3,t)) provisos (Bits#(t,tSz));
   // define your own 3-elements fifo here.
    Reg #(t) elements[3];
    for (Integer i = 0 ; i < 3 ;  i = i + 1)
    begin 
        elements[i] <- mkRegU;
    end
    Reg #(Bit#(2)) cur_start <- mkReg(0);
    Reg #(Bit#(2)) cur_next <- mkReg(0);
    Reg #(Bit#(2)) current_cap <- mkReg(0);

    function Bit#(2) next(Bit#(2) x);
        if (x == 2) begin return 0; end 
        else return x + 1;
    endfunction
    

    method t first;
        return elements[cur_start];
    endmethod

    method Bool notEmpty() ;
        return current_cap > 0;
    endmethod

    method Bool notFull() ;
        return current_cap < 3;
    endmethod

    method Action enq(t x) if (current_cap < 3);
        elements[cur_next] <= x;
        if (current_cap == 0) begin cur_start <= cur_next; end
        current_cap <= current_cap + 1;
        cur_next <= next(cur_next);
    endmethod

    method Action deq() if (current_cap > 0);
        cur_start <= next(cur_start);
        current_cap <= current_cap - 1;
    endmethod

endmodule


// Two elements conflict-free fifo given as black box
module mkCFFifo( Fifo#(2, t) ) provisos (Bits#(t, tSz));
    Ehr#(2, t) da <- mkEhr(?);
    Ehr#(2, Bool) va <- mkEhr(False);
    Ehr#(2, t) db <- mkEhr(?);
    Ehr#(2, Bool) vb <- mkEhr(False);

    rule canonicalize;
        if( vb[1] && !va[1] ) begin
            da[1] <= db[1];
            va[1] <= True;
            vb[1] <= False;
        end
    endrule

    method Action enq(t x) if(!vb[0]);
        db[0] <= x;
        vb[0] <= True;
    endmethod

    method Action deq() if(va[0]);
        va[0] <= False;
    endmethod

    method t first if (va[0]);
        return da[0];
    endmethod

    method Bool notEmpty();
        return va[0];
    endmethod

    method Bool notFull();
        return !vb[0];
    endmethod

endmodule

module mkCF3Fifo(Fifo#(3,t)) provisos (Bits#(t, tSz));
    FIFO#(t) bsfif <-  mkSizedFIFO(3);
    method Action enq( t x);
        bsfif.enq(x);
    endmethod

    method Action deq();
        bsfif.deq();
    endmethod

    method t first();
        return bsfif.first();
    endmethod

    method Bool notEmpty();
        return True;
    endmethod

    method Bool notFull();
        return True;
    endmethod

endmodule
