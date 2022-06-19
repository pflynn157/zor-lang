
machine run_counter1 is
    states S1, S2, S3;
    variable x : integer := 0;
    constant MAX : integer := 10;
begin    
    state S1 is
        println("Starting counter...");
        -> S2;
    end
    
    state S2 is
        if x = MAX then
            -> S3;
        else
            println(variable);
            variable := variable + 1;
            -> S2;
        end
    end
    
    state S3 is
        println("Done!");
        stop;
    end
end

algorithm add is
    input x : integer;
    input y : integer;
    output result : integer := 0;
begin
    result := x + y;
end

program main is
    input argc : integer;
    input argv : array<string>;

    variable result : integer := 0;
begin
    assert(argc > 1, "Error: Expected arguments.", fatal);
    assert(argc = 2, "You may need more args.", warning);
    assert(argc > 2, "Ah, perfect!", note);
    run_counter1();
    
    result := add(10, 20);
    println(result);
end

