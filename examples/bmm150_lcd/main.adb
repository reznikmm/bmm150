--  SPDX-FileCopyrightText: 2024 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

with Ada.Text_IO;

with Ravenscar_Time;

with STM32.Board;
with STM32.Device;
with STM32.GPIO;
with STM32.Setup;
with STM32.User_Button;

with HAL.Bitmap;
with HAL.Framebuffer;

with Display_ILI9341;
with Bitmapped_Drawing;
with BMP_Fonts;

with BMM150.I2C_Sensors;

with GUI;

procedure Main is
   use all type GUI.Button_Kind;

   Sensor : BMM150.I2C_Sensors.BMM150_Sensor
     (I2C_Port    => STM32.Device.I2C_1'Access,
      I2C_Address => 16#13#);

   procedure Configure_Sensor;
   --  Restart sensor with new settings according to GUI state

   subtype Sensor_Data is BMM150.Magnetic_Field_Vector;

   function Read_Sensor return BMM150.Magnetic_Field_Vector;

   function Min (Left, Right : Sensor_Data)
     return Sensor_Data is
       (X => BMM150.Magnetic_Field'Min (Left.X, Right.X),
        Y => BMM150.Magnetic_Field'Min (Left.Y, Right.Y),
        Z => BMM150.Magnetic_Field'Min (Left.Z, Right.Z));

   function Max (Left, Right : Sensor_Data)
     return Sensor_Data is
       (X => BMM150.Magnetic_Field'Max (Left.X, Right.X),
        Y => BMM150.Magnetic_Field'Max (Left.Y, Right.Y),
        Z => BMM150.Magnetic_Field'Max (Left.Z, Right.Z));

   use type BMM150.Magnetic_Field;

   function "*" (Percent : Integer; Right : Sensor_Data)
     return Sensor_Data is
       (X => BMM150.Magnetic_Field'Max
          (abs Right.X / 100, BMM150.Magnetic_Field'Small) * Percent,
        Y => BMM150.Magnetic_Field'Max
          (abs Right.Y / 100, BMM150.Magnetic_Field'Small) * Percent,
        Z => BMM150.Magnetic_Field'Max
          (abs Right.Z / 100, BMM150.Magnetic_Field'Small) * Percent);

   function "+" (Left, Right : Sensor_Data)
     return Sensor_Data is
       (X => Left.X + Right.X,
        Y => Left.Y + Right.Y,
        Z => Left.Z + Right.Z);

   type Sensor_Limits is record
      Min : Sensor_Data;
      Max : Sensor_Data;
   end record;

   procedure Make_Wider (Limits : in out Sensor_Limits);
   --  Make limits a bit wider

   procedure Print
     (LCD    : not null HAL.Bitmap.Any_Bitmap_Buffer;
      Data   : BMM150.Magnetic_Field_Vector);

   procedure Plot
     (LCD    : not null HAL.Bitmap.Any_Bitmap_Buffer;
      X      : Natural;
      Data   : in out Sensor_Data;
      Limits : Sensor_Limits);

   ----------------------
   -- Configure_Sensor --
   ----------------------

   procedure Configure_Sensor is
      Ok   : Boolean;
      Freq : BMM150.Output_Data_Rate := 2;
      Drv  : BMM150.Setting := BMM150.Low_Power_Preset;
      Map  : constant array (F2 .. F30) of BMM150.Output_Data_Rate
        := (2, 6, 8, 10, 15, 20, 25, 30);
   begin
      if GUI.State (+LP) then
         Drv := BMM150.Low_Power_Preset;
      elsif GUI.State (+RE) then
         Drv := BMM150.Regular_Preset;
      elsif GUI.State (+En) then
         Drv := BMM150.Enhanced_Regular_Preset;
      elsif GUI.State (+HA) then
         Drv := BMM150.High_Accuracy_Preset;
      end if;

      for F in Map'Range loop
         if GUI.State (+F) then
            Freq := Map (F);
            exit;
         end if;
      end loop;

      Sensor.Set_Power_Mode
        (Mode    => BMM150.Normal,
         ODR     => Freq,
         Success => Ok);
      pragma Assert (Ok);

      Sensor.Set_Repetitions (Drv, Ok);
      pragma Assert (Ok);
   end Configure_Sensor;

   ----------------
   -- Make_Wider --
   ----------------

      procedure Make_Wider (Limits : in out Sensor_Limits) is
   begin
      Limits.Min := Limits.Min + (-2) * Limits.Min;
      Limits.Max := Limits.Max + 2 * Limits.Max;
   end Make_Wider;

   -----------
   -- Print --
   -----------

   procedure Print
     (LCD  : not null HAL.Bitmap.Any_Bitmap_Buffer;
      Data : Sensor_Data)
   is
      TX : constant String := BMM150.Magnetic_Field'Image (Data.X);
      TY : constant String := BMM150.Magnetic_Field'Image (Data.Y);
      TZ : constant String := BMM150.Magnetic_Field'Image (Data.Z);
   begin
      if GUI.State (+Fx) then
         Bitmapped_Drawing.Draw_String
           (LCD.all,
            Start      => (0, 30),
            Msg        => TX,
            Font       => BMP_Fonts.Font8x8,
            Foreground => GUI.Buttons (+Fx).Color,
            Background => HAL.Bitmap.Black);
      end if;

      if GUI.State (+Fy) then
         Bitmapped_Drawing.Draw_String
           (LCD.all,
            Start      => (0, 40),
            Msg        => TY,
            Font       => BMP_Fonts.Font8x8,
            Foreground => GUI.Buttons (+Fy).Color,
            Background => HAL.Bitmap.Black);
      end if;

      if GUI.State (+Fz) then
         Bitmapped_Drawing.Draw_String
           (LCD.all,
            Start      => (0, 50),
            Msg        => TZ,
            Font       => BMP_Fonts.Font8x8,
            Foreground => GUI.Buttons (+Fz).Color,
            Background => HAL.Bitmap.Black);
      end if;
   end Print;

   ----------
   -- Plot --
   ----------

   procedure Plot
     (LCD    : not null HAL.Bitmap.Any_Bitmap_Buffer;
      X      : Natural;
      Data   : in out Sensor_Data;
      Limits : Sensor_Limits)
   is
      Small_4 : constant := 1.0 / 2.0 ** 4;

      type Fixed_4 is delta Small_4 range -2.0**19 .. 2.0**19 - Small_4;
      Height : constant Fixed_4 := Fixed_4 (LCD.Height);
      Value  : Fixed_4;
      Y      : Natural;
   begin
      Data := Min (Data, Limits.Max);
      Data := Max (Data, Limits.Min);

      if GUI.State (+Fx) then
         Value := Height * (Data.X - Limits.Min.X);
         Value := Value / (Limits.Max.X - Limits.Min.X);
         Y := Natural (Value);
         Y := LCD.Height - Y;
         LCD.Set_Pixel ((X, Y), GUI.Buttons (+Fx).Color);
      end if;

      if GUI.State (+Fy) then
         Value := Height * (Data.Y - Limits.Min.Y);
         Value := Value / (Limits.Max.Y - Limits.Min.Y);
         Y := Natural (Value);
         Y := LCD.Height - Y;
         LCD.Set_Pixel ((X, Y), GUI.Buttons (+Fy).Color);
      end if;

      if GUI.State (+Fz) then
         Value := Height * (Data.Z - Limits.Min.Z);
         Value := Value / (Limits.Max.Z - Limits.Min.Z);
         Y := Natural (Value);
         Y := LCD.Height - Y;
         LCD.Set_Pixel ((X, Y), GUI.Buttons (+Fz).Color);
      end if;
   end Plot;

   -----------------
   -- Read_Sensor --
   -----------------

   function Read_Sensor return BMM150.Magnetic_Field_Vector is
      Ok     : Boolean;
      Result : BMM150.Magnetic_Field_Vector;
   begin
      for J in 1 .. 2_000 loop
         exit when not Sensor.Measuring;
      end loop;

      Sensor.Read_Measurement (Result, Ok);
      pragma Assert (Ok);

      return Result;
   end Read_Sensor;

   LCD : constant not null HAL.Bitmap.Any_Bitmap_Buffer :=
     STM32.Board.TFT_Bitmap'Access;

   Next_Limits : Sensor_Limits;
begin
   STM32.Board.Initialize_LEDs;
   STM32.User_Button.Initialize;
   STM32.Board.Display.Initialize;
   STM32.Board.Display.Set_Orientation (HAL.Framebuffer.Landscape);
   STM32.Board.Touch_Panel.Initialize;
   STM32.Board.Touch_Panel.Set_Orientation (HAL.Framebuffer.Landscape);

   --  Initialize touch panel IRQ pin
   STM32.Board.TFT_RS.Configure_IO
     ((STM32.GPIO.Mode_In, Resistors => STM32.GPIO.Floating));

   STM32.Setup.Setup_I2C_Master
     (Port        => STM32.Device.I2C_1,
      SDA         => STM32.Device.PB9,
      SCL         => STM32.Device.PB8,
      SDA_AF      => STM32.Device.GPIO_AF_I2C1_4,
      SCL_AF      => STM32.Device.GPIO_AF_I2C1_4,
      Clock_Speed => 400_000);

   declare
      Ok : Boolean;
   begin
      Ravenscar_Time.Delays.Delay_Milliseconds (1);
      --  BMM150 power on reset time
      --  Power BMM150 on
      Sensor.Suspend_Off (Ravenscar_Time.Delays, Ok);
      pragma Assert (Ok);

      Sensor.Read_Trim_Registers (Ok);
      pragma Assert (Ok);
   end;

   --  Look for BMM150 chip
   if not Sensor.Check_Chip_Id then
      Ada.Text_IO.Put_Line ("BMM150 not found.");
      raise Program_Error;
   end if;

   Configure_Sensor;

   --  Predict boundaries from the first sensor measurement
   Next_Limits.Min := Read_Sensor;
   Next_Limits.Max := Next_Limits.Min;
   Make_Wider (Next_Limits);

   loop
      declare
         Data   : BMM150.Magnetic_Field_Vector;
         Update : Boolean := False;  --  GUI state updated
      begin
         GUI.Draw (LCD.all, Clear => True);  --  draw all buttons

         for X in 0 .. LCD.Width - 1 loop
            STM32.Board.Toggle (STM32.Board.D1_LED);

            Data := Read_Sensor;

            if not STM32.Board.TFT_RS.Set then  --  Touch IRQ Pin is active
               GUI.Check_Touch (STM32.Board.Touch_Panel, Update);
            end if;

            GUI.Draw (LCD.all);

            Next_Limits :=
              (Min => Min (Data, Next_Limits.Min),
               Max => Max (Data, Next_Limits.Max));

            Print (LCD, Data);
            Plot (LCD, X, Data, Next_Limits);

            if Update then
               Configure_Sensor;
               Update := False;
            elsif STM32.User_Button.Has_Been_Pressed then
               GUI.Dump_Screen (LCD.all);
            end if;
         end loop;
      end;
   end loop;
end Main;
