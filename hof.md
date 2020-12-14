Higher-Order Functions
=======================

Types
-----

    {
        "fun": {
            "params" : [<Type>*], 
            "ret" : <Type>?
        }
    }

Operations
----------

* `anon` : Indicates a function definition. 

* `apply` : Indicates a function application. The first argument is the function name. The subsequent arguments are arguments to that function.


add : fun<int, int>, <int> = anon(x, y) {res : int = x + y; ret res;};

res : int = apply add a b;

{
    "op": "anon",
    "args": ["x", "y"],
    "dest": "add",
    "type": {
        "fun": {
            "params" : ["int", "int"], 
            "ret" : "int"
        }
    },
    "instrs": [res : int = x + y, ret res]
}

{
    "op": "apply",
    "args": ["x", "y"],
    "dest": "res",
    "type": "int",
    "func": "add"
}

    x: int = const 6; 
    five: int = const 5;
    cond: bool = le x five; 
    br cond .y .z 
.y: 
    y: int = const 7; 
    jmp .rest;
.z: 
    z: int = const 7;
.rest: 
    f: fun<>, <int> = anon(x, y) {res: int = add x y; ret res;}
    a: int = fncall f; 

@g() {
    x: int = const 6; 
    five: int = const 5;
    cond: bool = le x five; 
    br cond .y .z 
.y: 
    y: int = const 7; 
    jmp .rest;
.z: 
    z: int = const 7;
.rest: 
    a: int = add x y;
    // a: int = call @f x y;
}
@f(x: int, y:int) {
    res: int = add x y;
    ret res; 
}
