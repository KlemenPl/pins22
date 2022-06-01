fun main() : void = {
    x[0][0] = 1;
    x[0][1] = 2;
    x[0][2] = 3;
    x[1][0] = 4;
    x[1][1] = 5;
    x[1][2] = 6;

    _putChar(x[0][0]);
    _putChar(x[0][1]);
    _putChar(x[0][2]);
    _putChar(x[1][0]);
    _putChar(x[1][1]);
    _putChar(x[1][2]);

    putChar((10 : char));

    ppX = (^(x[0][0]) : ^int);

    _putChar(funGetArrPtr(ppX, 0, 0)^);
    _putChar(funGetArrPtr(ppX, 0, 1)^);
    _putChar(funGetArrPtr(ppX, 0, 2)^);
    _putChar(funGetArrPtr(ppX, 1, 0)^);
    _putChar(funGetArrPtr(ppX, 1, 1)^);
    _putChar(funGetArrPtr(ppX, 1, 2)^);
    none;
};

var x : [2][3]int;
var ppX : ^int;

fun funGetArrPtr(ppArr : ^int, i : int, j : int) : ^int = (((ppArr : int) + i * 24 + j * 8) : ^int);

fun putChar(c : char) : void = none;
fun _putChar(c : int) : void = {
    putChar(((('0' : int) + c) : char));
    putChar((10 : char));
};