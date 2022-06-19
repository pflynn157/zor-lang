
algorithm sum is
    input x, y : integer;
    output result : integer := 0;
begin
    result := x + y;
end

test main is
begin
    for i in 0 to 100 do
        for j in 0 to 100 do
            assert(sum(x, y), "Error: invalid sum: " i + " " + j, fatal);
        end
    end
end

