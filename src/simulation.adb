-- A skeleton of a program for an assignment in programming languages
-- The students should rename the tasks of producers, consumers, and the buffer
-- Then, they should change them so that they would fit their assignments
-- They should also complete the code with constructions that lack there

-- Piotr Trybisz, 193557 mod 3 = 0
-- Pawel Pstragowski, 193473 mod 3 = 0

--TODO:
--[] Assembly (meal) alternatives, waiting for meal (not taking empty meal) - important!
----[] When primary assembly is ready give it
----[] When assembly alternative is ready give it
----[] Else wait for assembly (or alternative?) (up to X seconds?)
--[]Fancy dialogs
--[]More meals (assemblies)?
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;


procedure Simulation is
   Number_Of_Products: constant Integer := 8;
   Number_Of_Assemblies: constant Integer := 4;
   Number_Of_Consumers: constant Integer := 2;
   subtype Product_Type is Integer range 1 .. Number_Of_Products;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;
   function "+"(X: String) return Unbounded_String renames To_Unbounded_String;
   Product_Name: constant array (Product_Type) of Unbounded_String
     := (+"Bulka", +"Wolowina", +"Ser", +"Pikle", +"Salata", +"Kurczak", +"Pomidor", +"Bekon");
   --Sources:
   --1. https://mcdonalds.pl/nasze-menu/burgery/hamburger/
   --2. https://mcdonalds.pl/nasze-menu/burgery/cheeseburger/
   --3. https://mcdonalds.pl/nasze-menu/mcwrapy-i-salatki/mcwrap-klasyczny/
   --4. https://mcdonalds.pl/nasze-menu/mcwrapy-i-salatki/mcwrap-bekon-deluxe/
   Assembly_Name: constant array (Assembly_Type) of Unbounded_String
     := (+"Hamburger", +"Cheeseburger", +"McWrap Klasyczny", +"McWrap Bekon");
   Assembly_Alternatives: constant array (Assembly_Type) of Assembly_Type
     := (2,1,4,3);
   --Assembly contents defined in line 138
   package Random_Assembly is new
     Ada.Numerics.Discrete_Random(Assembly_Type);
   type My_Str is new String(1 ..256);

   -- Producer produces determined product
   task type Producer is
      -- Give the Producer an identity, i.e. the product type
      entry Start(Product: in Product_Type; Production_Time: in Integer);
      -- Pause task until resumed by buffer
      --entry Wait_For_Resume;
      -- Resumes production of previously producted product
      entry Resume_Production;
   end Producer;

   -- Consumer gets an arbitrary assembly of several products from the buffer
   task type Consumer is
      -- Give the Consumer an identity
      entry Start(Consumer_Number: in Consumer_Type;
                  Consumption_Time: in Integer);
   end Consumer;

   -- In the Buffer, products are assemblied into an assembly
   task type Buffer is
      -- Accept a product to the storage provided there is a room for it
      entry Take(Product: in Product_Type; Number: in Integer; Accepted: out Boolean);
      -- Deliver an assembly provided there are enough products for it
      entry Order(Assembly: in Assembly_Type; Number: out Integer);
   end Buffer;

   P: array ( 1 .. Number_Of_Products ) of Producer;
   K: array ( 1 .. Number_Of_Consumers ) of Consumer;
   B: Buffer;

   task body Producer is
      subtype Production_Time_Range is Integer range 3 .. 6;
      package Random_Production is new
        Ada.Numerics.Discrete_Random(Production_Time_Range);
      G: Random_Production.Generator;	--  generator liczb losowych
      Product_Type_Number: Integer;
      Product_Number: Integer;
      Production: Integer;
      Active: Boolean;
      Last_Accepted: Boolean;
   begin
      accept Start(Product: in Product_Type; Production_Time: in Integer) do
         Random_Production.Reset(G);	--  start random number generator
         Product_Number := 1;
         Product_Type_Number := Product;
         Production := Production_Time;
         Active := True;
      end Start;
      Put_Line("[PRODUCENT] Uruchomiono produkcje: " & Product_Name(Product_Type_Number));
      loop
         delay Duration(Random_Production.Random(G));
         Put_Line("[PRODUCENT] Wysylanie: " & Product_Name(Product_Type_Number)
                  & " #"  & Integer'Image(Product_Number));
         -- Accept for storage
         B.Take(Product_Type_Number, Product_Number, Last_Accepted);
         if not Last_Accepted then
            Put_Line("[PRODUCENT] Restauracja nie przyjela dostawy " & Product_Name(Product_Type_Number) & " - produkcja wstrzymana");
            accept Resume_Production  do
               Put_Line("[PRODUCENT] Restauracja jest gotowa przyjac " & Product_Name(Product_Type_Number) & " - wznawianie produkcji...");
            end Resume_Production;
         else
            Product_Number := Product_Number + 1;
         end if;
      end loop;
   end Producer;

   task body Consumer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new
        Ada.Numerics.Discrete_Random(Consumption_Time_Range);
      G: Random_Consumption.Generator;	--  random number generator (time)
      G2: Random_Assembly.Generator;	--  also (assemblies)
      Consumer_Nb: Consumer_Type;
      Assembly_Number: Integer;
      Consumption: Integer;
      Assembly_Type: Integer;
      Consumer_Name: constant array (1 .. Number_Of_Consumers)
        of String(1 .. 9)
        := ("Consumer1", "Consumer2");
   begin
      accept Start(Consumer_Number: in Consumer_Type;
                   Consumption_Time: in Integer) do
         Random_Consumption.Reset(G);	--  ustaw generator

         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;
      Put_Line("Started consumer " & Consumer_Name(Consumer_Nb));
      loop
         delay Duration(Random_Consumption.Random(G)); --  simulate consumption
         Assembly_Type := Random_Assembly.Random(G2);
         -- take an assembly for consumption
         Put_Line(Consumer_Name(Consumer_Nb) & ": ordered assembly " &
                    Assembly_Name(Assembly_Type));
         B.Order(Assembly_Type, Assembly_Number);
         --TODO: await order
         Put_Line(Consumer_Name(Consumer_Nb) & ": taken assembly " &
                    Assembly_Name(Assembly_Type) & " number " &
                    Integer'Image(Assembly_Number));
      end loop;
   end Consumer;

   task body Buffer is
      Storage_Capacity: constant Integer := 40;
      type Storage_type is array (Product_Type) of Integer;
      Storage: Storage_type
        := (0, 0, 0, 0, 0, 0, 0, 0);
      Assembly_Content: array(Assembly_Type, Product_Type) of Integer
        := ((2, 1, 0, 3, 0, 0, 0, 0),
            (2, 1, 1, 2, 0, 0, 0, 0),
            (0, 0, 3, 0, 2, 4, 4, 0),
            (0, 0, 0, 0, 2, 4, 4, 2));
      Max_Assembly_Content: array(Product_Type) of Integer;
      Assembly_Number: array(Assembly_Type) of Integer
        := (1, 1, 1, 1);
      In_Storage: Integer := 0;

      procedure Setup_Variables is
      begin
         for W in Product_Type loop
            Max_Assembly_Content(W) := 0;
            for Z in Assembly_Type loop
               if Assembly_Content(Z, W) > Max_Assembly_Content(W) then
                  Max_Assembly_Content(W) := Assembly_Content(Z, W);
               end if;
            end loop;
         end loop;
      end Setup_Variables;

      function Can_Accept(Product: Product_Type) return Boolean is
         Free: Integer;		--  free room in the storage
         -- how many products are for production of arbitrary assembly
         Lacking: array(Product_Type) of Integer;
         -- how much room is needed in storage to produce arbitrary assembly
         Lacking_room: Integer;
         MP: Boolean;			--  can accept
      begin
         Free := Storage_Capacity - In_Storage;
         if Free <= 0 then
            return False;
         end if;
         MP := True;
         for W in Product_Type loop
            if Storage(W) < Max_Assembly_Content(W) then
               MP := False;
            end if;
         end loop;
         if MP then
            return True;		--  storage has products for arbitrary
            --  assembly
         end if;
         if Integer'Max(0, Max_Assembly_Content(Product) - Storage(Product)) > 0 then
            -- exactly this product lacks
            --Put_Line("Exactly this product lacks: " & Product_Name(Product));
            return True;
         end if;
         Lacking_room := 1;			--  insert current product
         for W in Product_Type loop
            Lacking(W) := Integer'Max(0, Max_Assembly_Content(W) - Storage(W));
            Lacking_room := Lacking_room + Lacking(W);
         end loop;
         if Free >= Lacking_room then
            --Put_Line("There is enough room in storage for arbitrary assembly");
            -- there is enough room in storage for arbitrary assembly
            return True;
         else
            --Put_Line("No room for this product");
            -- no room for this product
            return False;
         end if;
      end Can_Accept;

      procedure Evaluate_Needs is
      begin
         for prod in Product_Type loop
            if Can_Accept(prod) then
               select
                  P(prod).Resume_Production;
               else
                  null;
                  --already working
               end select;
            end if;
         end loop;
         --Put_Line("Needs evaluated");
      end Evaluate_Needs;

      function Can_Deliver(Assembly: Assembly_Type) return Boolean is
      begin
         for W in Product_Type loop
            if Storage(W) < Assembly_Content(Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is
      begin
         for W in Product_Type loop
            Put_Line("Storage contents: " & Integer'Image(Storage(W)) & " "
                     & Product_Name(W));
         end loop;
      end Storage_Contents;

   begin
      Put_Line("Buffer started");
      Setup_Variables;
      loop
         select
            accept Take(Product: in Product_Type; Number: in Integer; Accepted: out Boolean) do
               if Can_Accept(Product) then
                  Put_Line("[RESTAURACJA] Przyjeto dostawe: " & Product_Name(Product) & " #" &
                             Integer'Image(Number));
                  Storage(Product) := Storage(Product) + 1;
                  In_Storage := In_Storage + 1;
                  Accepted:=True;
               else
                  Put_Line("[RESTAURACJA] Odrzucono dostawe: " & Product_Name(Product) & " #" &
                             Integer'Image(Number));
                  Accepted:=False;
               end if;
            end Take;
         or
            accept Order(Assembly: in Assembly_Type; Number: out Integer) do
               if Can_Deliver(Assembly) then
                  Put_Line("[RESTAURACJA] Delivered assembly " & Assembly_Name(Assembly) & " number " &
                             Integer'Image(Assembly_Number(Assembly)));
                  for W in Product_Type loop
                     Storage(W) := Storage(W) - Assembly_Content(Assembly, W);
                     In_Storage := In_Storage - Assembly_Content(Assembly, W);
                  end loop;
                  Number := Assembly_Number(Assembly);
                  Assembly_Number(Assembly) := Assembly_Number(Assembly) + 1;
               else
                  Put_Line("[RESTAURACJA] Lacking products for assembly " & Assembly_Name(Assembly));
                  Number := 0;
               end if;
               Evaluate_Needs;
            end Order;
         end select;
         Evaluate_Needs;
         --Storage_Contents;
      end loop;
   end Buffer;

begin
   for I in 1 .. Number_Of_Products loop
      P(I).Start(I, 0);
   end loop;
   for J in 1 .. Number_Of_Consumers loop
      K(J).Start(J,0);
   end loop;
end Simulation;


