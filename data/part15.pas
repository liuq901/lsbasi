program Part15;
var
    number: integer;
    a, b: integer;
    y: real;

procedure P1;
var
    a: real;
    k: integer;

    procedure P2;
    var
        a, z: integer;
    begin {P2}
        z := 777;
    end; {P2}

begin {p1}

end; {p1}

begin {Part15}
    number := 2;
    a := number;
    b := 10 * a + 10 * number div 4;
    y := 20 / 7 + 3.14
end. {Part15}
