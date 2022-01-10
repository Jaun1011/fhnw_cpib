
public class AllocBlockExec{
    public AllocBlockExec(int size) { }

    public void execute() throws ExecutionError{
        sp = sp + size;
        pc = pc + 1;
    }
}

public class EqIntExec{
    public void execute(){
        sp = sp - 1;
        store[sp - 1]= Data.intEQ(store[sp-1], store[sp]);
        pc= pc + 1;
    }
}


public class OutputBoolExec … {
    public OutputBoolExec(String indicator){}

    public void execute(){
        sp = sp - 1;
        boolean output= Data.boolGet(store[sp]);
        System.out.println("! " + indicator + " : bool = " + output);
        pc = pc + 1;
    }
}


public class InputBoolExec{
    public InputBoolExec(String indicator){}

    public void execute() throws ExecutionError{
        boolean input= InputUtility.readBool();

        int address= Data.intGet(store[sp - 1]);
        store[address]= Data.boolNew(input);
        sp= sp - 1;
        pc= pc + 1;
    }
}

public class LoadImIntExec{
    public LoadImIntExec(int value) {  }
    public void execute() throws ExecutionError{
        store[sp]= Data.intNew(value);
        sp= sp + 1;
        pc= pc + 1;
    }
}

public class LoadAddrRelExe{
    public LoadAddrRelExec(int relAddress){}

    public void execute() throws ExecutionError{
        store[sp] = Data.intNew(fp + relAddress);
        sp= sp + 1;
        pc= pc + 1;
    }
}

public class CallExec{
    public CallExec(int routAddress){ }
    public void execute() throws ExecutionError{
        store[sp]= Data.intNew(fp);
        store[sp + 1]= Data.intNew(ep);
        store[sp + 2]= Data.intNew(pc);
        
        fp= sp;
        sp= sp + 3;
        pc= routAddress;
    }
}

public class ReturnExec{
    public ReturnExec(int size) {}
    public void execute() throws ExecutionError
    {
        sp= fp - size;
        pc= Data.intGet(store[fp + 2]) + 1;
        ep= Data.intGet(store[fp + 1]);
        fp= Data.intGet(store[fp]);
    }
}




public class DerefExec{
    public void execute(){
        int address= Data.intGet(store[sp - 1]);
        store[sp - 1] = store[address];
        pc= pc + 1;
    }
}


public class StoreExec{

    /**
     * 
     *      1 a  *
     *      2 b
     *     *3   
     *      saves data[a] = b 
     * 
     */
    public void execute(){
        int address= Data.intGet(store[sp - 2]);
        store[address]= store[sp - 1];
        sp= sp - 2;
        pc= pc + 1;
    }
}


public class AddIntExec{
    public void execute(){
        sp = sp - 1;
        store[sp - 1]= Data.intAdd(store[sp-1], store[sp]);
        pc= pc + 1;
    }
}


public class UncondJumpExec{
    public UncondJumpExec(int jumpAddr){ }
    public void execute() {
        pc= jumpAddr;
    }
}



public class CondJumpExec{
    public CondJumpExec(int jumpAddr){}
    
    public void execute() {
        sp= sp - 1;
        pc= (Data.boolGet(store[sp])) ?
        pc + 1 : jumpAddr;
    }
}
