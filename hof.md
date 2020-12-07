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

add : fun<int, int>, <int> = anon(x, y) {res : int = x + y; ret res;};

res : int = call add a b;

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