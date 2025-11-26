with Ada.Text_IO;

procedure Greeter is
   Count : Integer := 5;
   
   procedure Greet_Times(N : Integer) is
   begin
      for I in 1 .. N loop
         Ada.Text_IO.Put_Line("Hello, World!");
      end loop;
   end Greet_Times;
   
begin
   Greet_Times(Count);
end Greeter;

