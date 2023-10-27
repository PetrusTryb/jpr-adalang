-- A skeleton of a program for an assignment in programming languages
-- The students should rename the tasks of producers, consumers, and the buffer
-- Then, they should change them so that they would fit their assignments
-- They should also complete the code with constructions that lack there

-- Piotr Trybisz, 193557 mod 3 = 0
-- Pawel Pstragowski, 193473 mod 3 = 0
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;


procedure Simulation is
   
   Number_Of_Products: constant Integer := 8;
   Number_Of_Meals: constant Integer := 4;
   Number_Of_Consumers: constant Integer := 2;
   subtype Product_Type is Integer range 1 .. Number_Of_Products;
   
   subtype Meal_Type is Integer range 1 .. Number_Of_Meals;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;
   
   function "+"(X: String) return Unbounded_String renames To_Unbounded_String;
   
   Product_Name: constant array (Product_Type) of Unbounded_String
     := (+"Hamburger Bun", +"Beef", +"Cheese", +"Pickles",
         +"Lettuce", +"Chicken", +"Tomato", +"Bacon");
   
   --Sources:
   --1. https://mcdonalds.pl/nasze-menu/burgery/hamburger/
   --2. https://mcdonalds.pl/nasze-menu/burgery/cheeseburger/
   --3. https://mcdonalds.pl/nasze-menu/mcwrapy-i-salatki/mcwrap-klasyczny/
   --4. https://mcdonalds.pl/nasze-menu/mcwrapy-i-salatki/mcwrap-bekon-deluxe/
   
   Meal_Name: constant array (Meal_Type) of Unbounded_String
     := (+"Hamburger", +"Cheeseburger", +"McWrap Classic", +"McWrap Bekon");
   
   --Meal contents defined in line 138
   package Random_Meal is new
     Ada.Numerics.Discrete_Random(Meal_Type);
   type My_Str is new String(1 ..256);

   -- Producer produces determined product
   task type Producer is
      -- Give the Producer an identity, i.e. the product type
      entry Start(Product: in Product_Type; Production_Time: in Integer);
      -- Resumes production of previously producted product
      entry Resume_Production;
   end Producer;

   -- Consumer gets an arbitrary Meal of several products from the buffer
   task type Consumer is
      -- Give the Consumer an identity
      entry Start(Consumer_Number: in Consumer_Type;
                  Consumption_Time: in Integer);
   end Consumer;
   
   task type Inspector is
      entry Start(Insp_Interval: in Integer; Max_Prods: in Integer);
   end Inspector;

   -- In the Buffer, products are assemblied into an Meal
   task type Buffer is
      -- Accept a product to the storage provided there is a room for it
      entry Take(Product: in Product_Type; Number: in Integer; Accepted: out Boolean);
      -- Deliver an Meal provided there are enough products for it
      entry Order(Meal: in Meal_Type; Number: out Integer);
      entry Inspection_In_Storage(MaxN: in Integer);
   end Buffer;

   P: array ( 1 .. Number_Of_Products ) of Producer;
   K: array ( 1 .. Number_Of_Consumers ) of Consumer;
   B: Buffer;
   I: Inspector;

   task body Producer is
      subtype Production_Time_Range is Integer range 4 .. 8;
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
      Put_Line("[PRODUCER] Production started: " & Product_Name(Product_Type_Number));
      loop
         delay Duration(Random_Production.Random(G));
         Put_Line("[PRODUCER] Sending: " & Product_Name(Product_Type_Number)
                  & " #"  & Integer'Image(Product_Number));
         -- Accept for storage
         B.Take(Product_Type_Number, Product_Number, Last_Accepted);
         
         if not Last_Accepted then
            Put_Line("[PRODUCER] Restaurant did not accept delivery "
                     & Product_Name(Product_Type_Number)
                     & " - production halted");
            accept Resume_Production  do
               Put_Line("[PRODUCER] Restaurant is ready to take order "
                        & Product_Name(Product_Type_Number)
                        & " - resuming production...");
            end Resume_Production;
         else
            Product_Number := Product_Number + 1;
         end if;
      end loop;
   end Producer;

   task body Consumer is
      subtype Consumption_Time_Range is Integer range 5 .. 10;
      package Random_Consumption is new
        Ada.Numerics.Discrete_Random(Consumption_Time_Range);
      G: Random_Consumption.Generator;	--  random number generator (time)
      G2: Random_Meal.Generator;	--  also (assemblies)
      Consumer_Nb: Consumer_Type;
      Meal_Number: Integer;
      Consumption: Integer;
      Meal_Type: Integer;
      Consumer_Name: constant array (1 .. Number_Of_Consumers)
        of String(1 .. 10)
        := ("CONSUMER_1", "CONSUMER_2");
   begin
      accept Start(Consumer_Number: in Consumer_Type;
                   Consumption_Time: in Integer) do
         Random_Consumption.Reset(G);	--  ustaw generator

         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;
      loop
         delay Duration(Random_Consumption.Random(G)); --  simulate consumption
         Meal_Type := Random_Meal.Random(G2);
         -- take an Meal for consumption
         Put_Line("["& Consumer_Name(Consumer_Nb) & "] I want to order: " &
                    Meal_Name(Meal_Type));
         B.Order(Meal_Type, Meal_Number);
         if Meal_Number > 0 then
            Put_Line("["&Consumer_Name(Consumer_Nb) & "] I picked up " &
                       Meal_Name(Meal_Type) & " #" &
                       Integer'Image(Meal_Number)&" - thanks!");
         else
            Put_Line("["&Consumer_Name(Consumer_Nb) & "] I will try next time.");
         end if;
      end loop;
   end Consumer;

   task body Buffer is
      Storage_Capacity: constant Integer := 64;
      type Storage_type is array (Product_Type) of Integer;
      
      Storage: Storage_type
        := (0, 0, 0, 0, 0, 0, 0, 0);
      
      Previously_Rejected: array (Product_Type) of Boolean
        := (False, False, False, False, False, False, False, False);
      
      Meal_Content: array(Meal_Type, Product_Type) of Integer
        := ((2, 1, 0, 3, 0, 0, 0, 0),
            (2, 1, 1, 2, 0, 0, 0, 0),
            (0, 0, 3, 0, 2, 4, 4, 0),
            (0, 0, 0, 0, 2, 4, 4, 2));
      
      Max_Meal_Content: array(Product_Type) of Integer;
      
      Meal_Number: array(Meal_Type) of Integer
        := (1, 1, 1, 1);
      In_Storage: Integer := 0;

      procedure Setup_Variables is
      begin
         for W in Product_Type loop
            Max_Meal_Content(W) := 0;
            for Z in Meal_Type loop
               if Meal_Content(Z, W) > Max_Meal_Content(W) then
                  Max_Meal_Content(W) := Meal_Content(Z, W);
               end if;
            end loop;
         end loop;
      end Setup_Variables;

      function Can_Accept(Product: Product_Type) return Boolean is
         Free: Integer;		--  free room in the storage
         -- how many products are for production of arbitrary Meal
         Lacking: array(Product_Type) of Integer;
         -- how much room is needed in storage to produce arbitrary Meal
         Lacking_room: Integer;
         MP: Boolean;			--  can accept
      begin
         Free := Storage_Capacity - In_Storage;
         if Free <= 0 then
            return False;
         end if;
         
         MP := True;
         
         for W in Product_Type loop
            if Storage(W) < Max_Meal_Content(W) then
               MP := False;
            end if;
         end loop;
         if MP then
            return True;		--  storage has products for arbitrary
            --  Meal
         end if;
         
         if Integer'Max(0, Max_Meal_Content(Product) - Storage(Product)) > 0 then
            -- exactly this product lacks
            return True;
         end if;
         Lacking_room := 1;			--  insert current product
         
         for W in Product_Type loop
            Lacking(W) := Integer'Max(0, Max_Meal_Content(W) - Storage(W));
            Lacking_room := Lacking_room + Lacking(W);
         end loop;
         
         if Free >= Lacking_room then
            -- there is enough room in storage for arbitrary Meal
            return True;
         else
            -- no room for this product
            return False;
         end if;
      end Can_Accept;

      procedure Evaluate_Needs is
      begin
         for prod in Product_Type loop
            if Previously_Rejected(prod) then
               if Can_Accept(prod) then
                  P(prod).Resume_Production;
                  Previously_Rejected(prod):=False;
               end if;
            end if;
         end loop;
      end Evaluate_Needs;

      function Can_Deliver(Meal: Meal_Type) return Boolean is
      begin
         for W in Product_Type loop
            if Storage(W) < Meal_Content(Meal, W) then
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
      
      procedure Inspection_process(maxN: in Integer) is
         All_OK: Boolean:=True;
      begin
         Storage_Contents;
         for W in Product_Type loop
            if Storage(W)>maxN then
               Put_Line("INSPECTION RESULT: You have more than "&Integer'Image(maxN)
                        &" products "& Product_Name(W) 
                        &" in your storage - you need to throw them away and clean up");
               Storage(W):=0;
               All_OK:=False;
            end if;
         end loop;
         if All_OK then
            Put_Line("INSPECTION RESULT: No problems");
         end if;
      end Inspection_process;

   begin
      Put_Line("[RESTAURANT] Welcome");
      Setup_Variables;
      loop
         select
            accept Take(Product: in Product_Type; Number: in Integer; Accepted: out Boolean) do
               if Can_Accept(Product) then
                  Put_Line("[RESTAURANT] Delivery accepted: "
                           & Product_Name(Product) & " #"
                           & Integer'Image(Number));
                  Storage(Product) := Storage(Product) + 1;
                  In_Storage := In_Storage + 1;
                  Accepted:=True;
               else
                  Put_Line("[RESTAURANT] Delivery rejected: " &
                             Product_Name(Product) & " #"
                           & Integer'Image(Number));
                  Accepted:=False;
                  Previously_Rejected(Product):=True;
               end if;
            end Take;
         or
            accept Order(Meal: in Meal_Type; Number: out Integer) do
               if Can_Deliver(Meal) then
                  Put_Line("[RESTAURANT] Order: "
                           & Meal_Name(Meal) & " #"
                           & Integer'Image(Meal_Number(Meal)) &" is ready.");
                  for W in Product_Type loop
                     Storage(W) := Storage(W) - Meal_Content(Meal, W);
                     In_Storage := In_Storage - Meal_Content(Meal, W);
                  end loop;
                  
                  Number := Meal_Number(Meal);
                  Meal_Number(Meal) := Meal_Number(Meal) + 1;
               else
                  Put_Line("[RESTAURANT] Sorry, we do not currently have "
                           & Meal_Name(Meal) & " in stock.");
                  Number := 0;
               end if;
            end Order;
         or
            accept Inspection_In_Storage(maxN: Integer) do
               Inspection_process(maxN);
               Evaluate_Needs;
            end Inspection_In_Storage;
         end select;
         Evaluate_Needs;
      end loop;
   end Buffer;
   
   task body Inspector is
      Interval: Integer;
      Max_Prod: Integer;
   begin
      accept Start(Insp_Interval: in Integer; Max_Prods: in Integer) do
         Interval:=Insp_Interval;
         Max_Prod:=Max_Prods;
         Put_Line("[INSPECTOR] Started");
      end Start;
      loop
         delay Duration(Interval);
         Put_Line("[INSPECTOR] Inspecting buffer...");
         B.Inspection_In_Storage(Max_Prod);
         Put_Line("[INSPECTOR] Inspection finished");
      end loop;
   end Inspector;

begin
   for I in 1 .. Number_Of_Products loop
      P(I).Start(I, 0);
   end loop;
   for J in 1 .. Number_Of_Consumers loop
      K(J).Start(J,0);
   end loop;
   --delay 7 seconds, max 5 products
   I.Start(7,5);
end Simulation;


