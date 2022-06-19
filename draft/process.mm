
process keyreader is
    input x : character;
begin
    if x = '\n' then
        println("Newline");
    else
        println("Other char");
    end
end

## Not sure yet to hook up a process
program main is
begin
    println("Main");
end

