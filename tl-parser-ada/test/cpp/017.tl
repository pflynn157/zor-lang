func main -> i32 is
    var n : i32[10];
    var i : i32 := 5;
    
    var x1 : i32 := n[2];
    var y1 : i32 := n[i];
    var z1 : i32 := n[i * 2];
    var x2 : i32 := 1 + n[2] * 3;
    var y2 : i32 := 2 + n[i] * 4;
    var z2 : i32 := 5 + n[i * 2] * 6;
    return 0;
end
