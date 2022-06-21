struct S1 is
    x : i32 := 10;
    y : i32 := 20;
end
func main -> i32 is
    struct s : S1;
    s.x := 1;
    var x : i32 := s.y;
    var y : i32 := s.y + 20;
    return 0;
end
