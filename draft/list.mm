
machine triple_list is
    states Begin, Fill, Full
    inout list : array<integer>;
    variable i : integer := 0;
begin
    state Begin is
        i := 0;
    end
    
    state Fill is
        list[i] := i * 3;
        if i < length(list) then
            i := i + 1;
            -> Fill;
        else
            -> Full;
        end
    end
    
    state Full is
        stop;
    end
end

algorithm reduce_list is
    input list : array<integer>;
    output sum : integer := 0;
begin
    foreach num in list do
        sum := sum + num;
    end
end

algorithm fill_list is
    inout list : array<integer>;
begin
    for i in 0 to length(list) do
        list[i] := i;
    end
end

algorithm double_list is
    inout list : array<integer>;
    variable i : integer := 0;
begin
    while i < length(list) do
        list[i] := i * 2;
        i := i + 1;
    end
end

program main is
    variable list : array<integer>(10);
begin
    fill_list(list);
end

