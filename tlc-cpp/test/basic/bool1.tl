
#OUTPUT
#True
#END

#RET 0

#import std.io;

extern println(ln:string);

func main -> i32 is
    var x : bool := true;
    
    if x then println("True");
    else println("False");
    end
    
    return 0;
end
