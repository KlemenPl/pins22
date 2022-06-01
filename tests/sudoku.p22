#{
# Sudoku solved using backtracking.
#}#

fun main() : int = ({
    #{ Clear puzzle }#
    i = 0;
    while i < 81 do
        puzzle[i] = 0;
        i = i + 1;
    end;
    #{ Setting initial state. }#
    puzzle[0 * 9 + 2] = 6;
    puzzle[0 * 9 + 3] = 9;

    puzzle[1 * 9 + 1] = 3;
    puzzle[1 * 9 + 4] = 5;
    puzzle[1 * 9 + 7] = 2;
    puzzle[1 * 9 + 8] = 8;

    puzzle[2 * 9 + 6] = 4;

    puzzle[3 * 9 + 0] = 8;
    puzzle[3 * 9 + 5] = 5;

    puzzle[4 * 9 + 8] = 7;

    puzzle[5 * 9 + 1] = 2;
    puzzle[5 * 9 + 4] = 8;
    puzzle[5 * 9 + 7] = 1;
    puzzle[5 * 9 + 8] = 3;

    puzzle[6 * 9 + 1] = 4;
    puzzle[6 * 9 + 6] = 5;

    puzzle[7 * 9 + 4] = 1;
    puzzle[7 * 9 + 6] = 7;

    puzzle[8 * 9 + 2] = 3;
    puzzle[8 * 9 + 5] = 6;
    puzzle[8 * 9 + 7] = 4;
    puzzle[8 * 9 + 8] = 1;


    #{ Print starting configuration }#
    printPuzzle((^puzzle : ^int));
    putChar(10);

    #{ Print solved puzzle }#
    solved = solve((^puzzle : ^int));
    printPuzzle(solved);

    del (solved : ^void);
    0;
} where
    var puzzle : [81]int;
    var solved : ^int;
    var i : int;
);

fun solve(puzzle : ^int) : ^int = ({
    ret = (nil : ^int);
    #{ First pass: write directly, if only one candidate applies. }#
    anotherPass = 1;
    invalidState = 0;
    while !invalidState & anotherPass do
        minIdx = -1;
        anotherPass = 0;
        numMinCandidates = -1;

        i = 0;
        while !invalidState & i < 81 do
            if arrIdx(puzzle, i)^ == 0 then
                numCandidates = getCandidatesI(i);
                if numCandidates == 0 then
                    #{ Invalid state, no possible solution }#
                    invalidState = 1;
                end;

                if !invalidState then
                    if numCandidates == 1 then
                        arrIdx(puzzle, i)^ = candidates[0];
                        anotherPass = 1;
                    else
                        if numMinCandidates == -1 | numCandidates < numMinCandidates then
                            memcpy((^minCandidates : ^int), (^candidates : ^int), 9);
                            numMinCandidates = numCandidates;
                            minIdx = i;
                        end;
                    end;
                end;
            end;
            i = i + 1;
        end;
    end;

    if !invalidState then
        if numMinCandidates == -1 then
            #{ Puzzle is solved. }#
            anotherPass = 0;
            ret = puzzle;
        else
            #{ Now we try solving spot with the fewest possible candidates. }#
            i = 0;
            while i < numMinCandidates do
                ({
                    newPuzzle = (new (81 * 8) : ^int);
                    memcpy(newPuzzle, puzzle, 81);

                    arrIdx(newPuzzle, minIdx)^ = minCandidates[i];
                    ret = solve(newPuzzle);
                    if !(ret : int) then
                        del (newPuzzle : ^void);
                    else
                        i = 10;
                    end;
                } where
                    var newPuzzle : ^int;
                );
                i = i + 1;

            end;
        end;
    end;

    ret;
} where
    var i : int;
    var j : int;

    var anotherPass : int;
    var minIdx : int;

    var ret : ^int;
    var invalidState : int;

    var candidates : [9]int;
    var minCandidates : [9]int;
    var numMinCandidates : int;
    var numCandidates : int;

    fun getCandidatesI(idx : int) : int = getCandidates(idx % 9, idx / 9);
    fun getCandidates(x : int, y : int) : int = ({
        if getVal(puzzle, x, y)^ != 0 then
            ret = 0;
        else
            i = 0;
            while i < 9 do
                candidates[i] = i + 1;
                i = i + 1;
            end;

            #{ Eliminate horizontal candidates. }#
            i = 0;
            while i < 9 do
                if i != x then
                    val = getVal(puzzle, i, y)^;
                    eliminateCandidate(val);
                end;
                i = i + 1;
            end;

            #{ Eliminate vertical candidates. }#
            i = 0;
            while i < 9 do
                if i != y then
                    val = getVal(puzzle, x, i)^;
                    eliminateCandidate(val);
                end;
                i = i + 1;
            end;

            #{ Eliminate box candidates. }#
            i = 0;
            while i < 3 do
                j = 0;
                while j < 3 do
                    ({
                        x0 = (x / 3) * 3 + j;
                        y0 = (y / 3) * 3 + i;
                        if !(x0 == x & y0 == y) then
                            val = getVal(puzzle, x0, y0)^;
                            eliminateCandidate(val);
                        end;
                    } where
                        var x0 : int;
                        var y0 : int;
                    );
                    j = j + 1;
                end;
                i = i + 1;
            end;

            #{ Write only viable candidates }#
            ({
                ret = 0;
                i = 0;
                while i < 9 do
                    if candidates[i] then
                        tmp[ret] = candidates[i];
                        ret = ret + 1;
                    end;
                    i = i + 1;
                end;

                i = 0;
                j = 0;
                while i < 9 do
                    if tmp[i] then
                        candidates[j] = tmp[i];
                        j = j + 1;
                    end;
                    i = i + 1;
                end;
            } where
                var tmp : [9] int;
            );
        end;
        ret;
    } where
        var i : int;
        var j : int;
        var val : int;
        var ret : int;

        fun eliminateCandidate(val : int) : void = {
            if val then
                candidates[val - 1] = 0;
            end;
        };
    );
);

#{
fun printPuzzle(puzzle : ^int) : void = ({
    #{ ╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗ }#
    putChar(9556);putChar(9552);putChar(9552);putChar(9552);
    putChar(9572);putChar(9552);putChar(9552);putChar(9552);
    putChar(9572);putChar(9552);putChar(9552);putChar(9552);
    putChar(9574);putChar(9552);putChar(9552);putChar(9552);
    putChar(9572);putChar(9552);putChar(9552);putChar(9552);
    putChar(9572);putChar(9552);putChar(9552);putChar(9552);
    putChar(9574);putChar(9552);putChar(9552);putChar(9552);
    putChar(9572);putChar(9552);putChar(9552);putChar(9552);
    putChar(9572);putChar(9552);putChar(9552);putChar(9552);
    putChar(9559);putChar(10);

    i = 0;
    while i < 9 do
        j = 0;
        while j < 9 do
            if j % 3 then
                putChar(9474);
            else
                putChar(9553);
            end;
            val = getVal(puzzle, j, i)^;
            if !val then
                putChar((' ' : int));putChar((' ' : int));putChar((' ' : int));
            else
                putChar((' ' : int));putChar(('0' : int) + val);putChar((' ' : int));
            end;
            j = j + 1;
        end;
        putChar(9553);
        putChar(10);

        if i + 1 < 9 then
            if i == 2 | i == 5 then
                putChar(9568);putChar(9552);putChar(9552);putChar(9552);
                putChar(9578);putChar(9552);putChar(9552);putChar(9552);
                putChar(9578);putChar(9552);putChar(9552);putChar(9552);
                putChar(9580);putChar(9552);putChar(9552);putChar(9552);
                putChar(9578);putChar(9552);putChar(9552);putChar(9552);
                putChar(9578);putChar(9552);putChar(9552);putChar(9552);
                putChar(9580);putChar(9552);putChar(9552);putChar(9552);
                putChar(9578);putChar(9552);putChar(9552);putChar(9552);
                putChar(9578);putChar(9552);putChar(9552);putChar(9552);
                putChar(9571);putChar(10);
            else
                putChar(9567);putChar(9472);putChar(9472);putChar(9472);
                putChar(9532);putChar(9472);putChar(9472);putChar(9472);
                putChar(9532);putChar(9472);putChar(9472);putChar(9472);
                putChar(9579);putChar(9472);putChar(9472);putChar(9472);
                putChar(9532);putChar(9472);putChar(9472);putChar(9472);
                putChar(9532);putChar(9472);putChar(9472);putChar(9472);
                putChar(9579);putChar(9472);putChar(9472);putChar(9472);
                putChar(9532);putChar(9472);putChar(9472);putChar(9472);
                putChar(9532);putChar(9472);putChar(9472);putChar(9472);
                putChar(9570);putChar(10);
            end;
        end;
        i = i + 1;
    end;
    putChar(9562);putChar(9552);putChar(9552);putChar(9552);
    putChar(9575);putChar(9552);putChar(9552);putChar(9552);
    putChar(9575);putChar(9552);putChar(9552);putChar(9552);
    putChar(9577);putChar(9552);putChar(9552);putChar(9552);
    putChar(9575);putChar(9552);putChar(9552);putChar(9552);
    putChar(9575);putChar(9552);putChar(9552);putChar(9552);
    putChar(9577);putChar(9552);putChar(9552);putChar(9552);
    putChar(9575);putChar(9552);putChar(9552);putChar(9552);
    putChar(9575);putChar(9552);putChar(9552);putChar(9552);
    putChar(9565);putChar(10);
} where
    var i : int;
    var j : int;
    var val : int;
);
}#

fun printPuzzle(puzzle : ^int) : void = ({
    i = 0;
    while i < 37 do
        putChar(('#' : int));
        i = i + 1;
    end;
    putChar(10);

    i = 0;
    while i < 9 do
        j = 0;
        while j < 9 do
            if j % 3 == 0 then
                putChar(('#' : int));
            else
                putChar(('|' : int));
            end;
            val = getVal(puzzle, j, i)^;
            if !val then
                putChar((' ' : int));putChar((' ' : int));putChar((' ' : int));
            else
                putChar((' ' : int));putChar(('0' : int) + val);putChar((' ' : int));
            end;
            j = j + 1;
        end;
        putChar(('#' : int));
        putChar(10);

        if i + 1 < 9 then
            j = 0;
            while j < 37 do
                if i == 2 | i == 5 then
                    putChar(('#' : int));
                else
                    if j % 12 == 0 then
                        putChar(('#' : int));
                    else
                        putChar(('-' : int));
                    end;
                end;
                j = j + 1;
            end;
            putChar(10);
        end;
        i = i + 1;
    end;
    i = 0;
    while i < 37 do
        putChar(('#' : int));
        i = i + 1;
    end;
    putChar(10);
} where
    var i : int;
    var j : int;
    var val : int;
);

fun memcpy(dst : ^int, src : ^int, size : int) : void = ({
    i = 0;
    while i < size do
        (((dst : int) + 8 * i) : ^int)^ = (((src : int) + 8 * i) : ^int)^;
        i = i + 1;
    end;
} where
    var i : int;
);

fun arrIdx(arr : ^int, i : int) : ^int = (((arr : int) + i * 8 ) : ^int);
fun getVal(arr : ^int, x : int, y : int) : ^int = (((arr : int) + y * 9 * 8 + x * 8 ) : ^int);
fun putChar(c : int) : void = none;
fun exit() : void = none;