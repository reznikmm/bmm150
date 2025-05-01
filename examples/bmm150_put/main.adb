--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

with Ada.Real_Time;
with Ada.Text_IO;

with Ravenscar_Time;

with STM32.Board;
with STM32.Device;
with STM32.Setup;

with BMM150.I2C;

procedure Main is
   use type Ada.Real_Time.Time;

   package BMM150_I2C is new BMM150.I2C
     (I2C_Port    => STM32.Device.I2C_1'Access,
      I2C_Address => 16#13#);

   Next : Ada.Real_Time.Time := Ada.Real_Time.Clock;

   Ok    : Boolean;
   Trim  : BMM150.Trim_Registers;
   Value : BMM150.Optional_Magnetic_Field_Vector;

begin
   STM32.Board.Initialize_LEDs;
   STM32.Setup.Setup_I2C_Master
     (Port        => STM32.Device.I2C_1,
      SDA         => STM32.Device.PB9,
      SCL         => STM32.Device.PB8,
      SDA_AF      => STM32.Device.GPIO_AF_I2C1_4,
      SCL_AF      => STM32.Device.GPIO_AF_I2C1_4,
      Clock_Speed => 100_000);

   Ravenscar_Time.Delays.Delay_Milliseconds (1);
   --  BMM150 power on reset time

   --  Power BMM150 on
   BMM150_I2C.Suspend_Off (Ravenscar_Time.Delays, Ok);
   pragma Assert (Ok);

   --  Look for BMM150 chip
   if not BMM150_I2C.Check_Chip_Id then
      Ada.Text_IO.Put_Line ("BMM150 not found.");
      raise Program_Error;
   end if;

   --  Reset BMM150
   BMM150_I2C.Reset (Ok);
   pragma Assert (Ok);

   --  Read calibration data into Clib
   BMM150_I2C.Read_Trim_Registers (Trim, Ok);

   --  Set the number of repetitions on X/Y and Z axes
   BMM150_I2C.Set_Repetitions
     (Preset  => BMM150.Low_Power_Preset,
      Success => Ok);
   pragma Assert (Ok);

   --  Enable contigous measurement mode
   BMM150_I2C.Set_Power_Mode
     (Mode    => BMM150.Normal,
      Success => Ok);
   pragma Assert (Ok);

   loop
      STM32.Board.Toggle (STM32.Board.D1_LED);

      BMM150_I2C.Read_Measurement (Value, Trim, Ok);

      if Ok then
         if BMM150.Has_Overflow (Value) then
            Ada.Text_IO.Put_Line ("Overflow!");
         else
            declare
               V : constant BMM150.Magnetic_Field_Vector :=
                 BMM150.To_Magnetic_Field_Vector (Value);
            begin
               Ada.Text_IO.Put_Line
                 (V.X'Image & ", " & V.Y'Image & ", " & V.Z'Image);
            end;
         end if;
      end if;

      Next := Next + Ada.Real_Time.Milliseconds (500);
      delay until Next;
   end loop;
end Main;
