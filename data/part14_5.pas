program Scope5;
    var x, y: real;

    procedure alpha(a: integer);
        var y: integer;
        a: real; {ERROR here}
    begin
        x := a + x + y;
    end;

begin {Main}
end. {Main}
