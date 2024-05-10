import Ehr::*;
import Vector::*;

interface Fifo#(numeric type n, type t);
    method Bool notFull;
    method Action enq(t x);
    method Bool notEmpty;
    method Action deq;
    method t first;
    method Action clear;
endinterface

module mkMyConflictFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Reg#(t))     data     <- replicateM(mkRegU());
    Reg#(Bit#(TLog#(n)))    enqP     <- mkReg(0);
    Reg#(Bit#(TLog#(n)))    deqP     <- mkReg(0);
    Reg#(Bool)              isEmpty <- mkReg(True);
    Reg#(Bool)              isFull  <- mkReg(False);
    Bit#(TLog#(n))          size     = fromInteger(valueOf(n)-1);

    method Bool notFull();
        return !isFull;
    endmethod

    method Action enq (t x) if (!isFull);
        data[enqP] <= x;

        Bit#(TLog#(n)) next_enqP = (enqP == size) ? 0 : enqP + 1;
        if (next_enqP == deqP) isFull <= True;
        enqP <= next_enqP;
        isEmpty <= False;
    endmethod

    method Bool notEmpty();
        return !isEmpty;
    endmethod

    method Action deq() if (!isEmpty);
        Bit#(TLog#(n)) next_deqP = (deqP == size) ? 0 : deqP + 1;
        
        if (enqP == next_deqP) isEmpty <= True;
        deqP <= next_deqP;
        isFull <= False;
    endmethod

    method t first() if (!isEmpty);
        return data[deqP];
    endmethod

    method Action clear();
        enqP <= 0;
        deqP <= 0;
        isEmpty <= True;
        isFull <= False;
    endmethod

endmodule

// {notEmpty, first, deq} < {notFull, enq} < clear
module mkMyPipelineFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Reg#(t))     data     <- replicateM(mkRegU());
    Ehr#(3, Bit#(TLog#(n))) enqP     <- mkEhr(0);
    Ehr#(3, Bit#(TLog#(n))) deqP     <- mkEhr(0);
    Ehr#(3, Bool)           nEmpty <- mkEhr(False);
    Ehr#(3, Bool)           nFull  <- mkEhr(True);
    Bit#(TLog#(n))          size     = fromInteger(valueOf(n)-1);

    // 0

    method Bool notEmpty();
        return nEmpty[0];
    endmethod

    method Action deq() if (nEmpty[0]);
        Bit#(TLog#(n)) next_deqP = (deqP[0] == size) ? 0 : deqP[0] + 1;
            
        if (enqP[0] == next_deqP) nEmpty[0] <= False;
        deqP[0] <= next_deqP;
        nFull[0] <= True;
        //$display("%d , %d \n" , deqP[0] , enqP[0]);
    endmethod

    method t first() if (nEmpty[0]);
        return data[deqP[0]];
    endmethod

    // 1

    method Bool notFull();
       return nFull[1];
    endmethod

    method Action enq (t x) if (nFull[1]);
        data[enqP[1]] <= x;

        Bit#(TLog#(n)) next_enqP = (enqP[1] == size) ? 0 : enqP[1] + 1;
        if (next_enqP == deqP[1]) nFull[1] <= False;
        enqP[1] <= next_enqP;
        nEmpty[1] <= True;
    endmethod

    

    // 2

    method Action clear();
        enqP[2] <= 0;
        deqP[2] <= 0;
        nEmpty[2] <= False;
        nFull[2] <= True;
    endmethod

endmodule

// {notFull, enq} < {notEmpty, first, deq} < clear
module mkMyBypassFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Ehr#(2, t))  data     <- replicateM(mkEhrU());
    Ehr#(3, Bit#(TLog#(n))) enqP     <- mkEhr(0);
    Ehr#(3, Bit#(TLog#(n))) deqP     <- mkEhr(0);
    Ehr#(3, Bool)           nEmpty <- mkEhr(False);
    Ehr#(3, Bool)           nFull  <- mkEhr(True);
    Bit#(TLog#(n))          size     = fromInteger(valueOf(n)-1);

    // 0

    method Bool notFull();
        return nFull[0];
    endmethod

    method Action enq (t x) if (nFull[0]);
        data[enqP[0]][0] <= x;

        Bit#(TLog#(n)) next_enqP = (enqP[0] == size) ? 0 : enqP[0] + 1;
        if (next_enqP == deqP[0]) nFull[0] <= False;
        enqP[0] <= next_enqP;
        nEmpty[0] <= True;
    endmethod

    // 1

    method Bool notEmpty();
        return nEmpty[1];
    endmethod

    method Action deq() if (nEmpty[1]);
        Bit#(TLog#(n)) next_deqP = (deqP[1] == size) ? 0 : deqP[1] + 1;

        if (enqP[1] == next_deqP) nEmpty[1] <= False;
        deqP[1] <= next_deqP;
        nFull[1] <= True;
    endmethod

    method t first() if (nEmpty[1]);
        return data[deqP[1]][1];
    endmethod

    // 2

    method Action clear();
        enqP[1] <= 0;
        deqP[1] <= 0;
        nEmpty[1] <= False;
        nFull[1] <= True;
    endmethod
endmodule

// {notFull, enq, notEmpty, first, deq} < clear
module mkMyCFFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Reg#(t))     data         <- replicateM(mkRegU());
    Ehr#(2, Bit#(TLog#(n))) enqP         <- mkEhr(0);
    Ehr#(2, Bit#(TLog#(n))) deqP         <- mkEhr(0);
    Ehr#(2, Bool)           nEmpty     <- mkEhr(False);
    Ehr#(2, Bool)           nFull      <- mkEhr(True);
    Ehr#(2, Bool)           req_deq      <- mkEhr(False);
    Ehr#(2, Maybe#(t))      req_enq      <- mkEhr(tagged Invalid);
    Bit#(TLog#(n))          size         = fromInteger(valueOf(n)-1);

    (*no_implicit_conditions, fire_when_enabled*)
    rule canonicalize;
        // enq and deq
        Bit#(TLog#(n)) next_enqP = (enqP[0] == size) ? 0 : enqP[0] + 1;
        Bit#(TLog#(n)) next_deqP = (deqP[0] == size) ? 0 : deqP[0] + 1;
        if (isValid(req_enq[1]) && req_deq[1]) // not full and not empty
        begin
            data[enqP[0]] <= fromMaybe(? , req_enq[1]);
            enqP[0] <= next_enqP;
            deqP[0] <= next_deqP;
        end
        else if (req_deq[1])
        begin
            if (enqP[0] == next_deqP) nEmpty[0] <= False;
            nFull[0] <= True;
            deqP[0] <= next_deqP;
        end
        else if (isValid(req_enq[1]))
        begin 
            data[enqP[0]] <= fromMaybe(? , req_enq[1]);
            if (next_enqP == deqP[0]) nFull[0] <= False;
            nEmpty[0] <= True;
            enqP[0] <= next_enqP;
        end
        
   
        req_enq[1] <= tagged Invalid;
        req_deq[1] <= False;
    endrule

    method Bool notFull();
        return nFull[0];
    endmethod

    method Action enq (t x) if (nFull[0]);
        req_enq[0] <= tagged Valid x;
    endmethod

    method Bool notEmpty();
        return nEmpty[0];
    endmethod

    method Action deq() if (nEmpty[0]);
        req_deq[0] <= True;
    endmethod

    method t first() if (nEmpty[0]);
        return data[deqP[0]];
    endmethod

    method Action clear();
        enqP[1] <= 0;
        deqP[1] <= 0;
        nEmpty[1] <= False;
        nFull[1] <= True;
    endmethod

endmodule
