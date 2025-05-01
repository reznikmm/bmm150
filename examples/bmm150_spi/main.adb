--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Text_IO;
with Interfaces;

with Ravenscar_Time;

with HAL.SPI;

with STM32.Board;
with STM32.Device;
with STM32.GPIO;
with STM32.SPI;

with BMM150.SPI;

procedure Main is

   SPI      renames STM32.Device.SPI_2;
   SPI_SCK  renames STM32.Device.PB13;
   SPI_MISO renames STM32.Device.PB14;
   SPI_MOSI renames STM32.Device.PB15;
   SPI_CS   renames STM32.Device.PB10;
   SPI_CS2  renames STM32.Device.PB11;

   package BMM150_SPI is new BMM150.SPI
     (SPI_Port => SPI'Access,
      SPI_CS   => SPI_CS'Access);

   procedure Setup_SPI_2;

   -----------------
   -- Setup_SPI_2 --
   -----------------

   procedure Setup_SPI_2 is

      SPI_Pins : constant STM32.GPIO.GPIO_Points :=
        [SPI_SCK, SPI_MISO, SPI_MOSI, SPI_CS];
   begin
      STM32.Device.Enable_Clock (SPI_Pins);

      STM32.GPIO.Configure_IO
        (SPI_CS,
         (Mode        => STM32.GPIO.Mode_Out,
          Resistors   => STM32.GPIO.Floating,
          Output_Type => STM32.GPIO.Push_Pull,
          Speed       => STM32.GPIO.Speed_100MHz));

      SPI_CS.Set;

      STM32.GPIO.Configure_IO
        (SPI_CS2,
         (Mode        => STM32.GPIO.Mode_Out,
          Resistors   => STM32.GPIO.Floating,
          Output_Type => STM32.GPIO.Push_Pull,
          Speed       => STM32.GPIO.Speed_100MHz));

      SPI_CS2.Set;

      STM32.GPIO.Configure_IO
        (SPI_Pins (1 .. 3),
         (Mode           => STM32.GPIO.Mode_AF,
          Resistors      => STM32.GPIO.Pull_Up,
          AF_Output_Type => STM32.GPIO.Push_Pull,
          AF_Speed       => STM32.GPIO.Speed_100MHz,
          AF             => STM32.Device.GPIO_AF_SPI2_5));

      STM32.Device.Enable_Clock (SPI);

      STM32.SPI.Configure
        (SPI,
         (Direction           => STM32.SPI.D2Lines_FullDuplex,
          Mode                => STM32.SPI.Master,
          Data_Size           => HAL.SPI.Data_Size_8b,
          Clock_Polarity      => STM32.SPI.High,   --   Mode 3
          Clock_Phase         => STM32.SPI.P2Edge,
          Slave_Management    => STM32.SPI.Software_Managed,
          Baud_Rate_Prescaler => STM32.SPI.BRP_8,
          First_Bit           => STM32.SPI.MSB,
          CRC_Poly            => 0));
      --  SPI2 sits on APB1, which is 42MHz, so SPI rate in 42/8=5.25MHz
   end Setup_SPI_2;

   Ok     : Boolean := False;
   Trim   : BMM150.Trim_Registers;
   R_Hall : Interfaces.Unsigned_16;
   Vector : array (1 .. 16) of BMM150.Raw_Density_Vector;

begin
   STM32.Board.Initialize_LEDs;
   Setup_SPI_2;

   Ravenscar_Time.Delays.Delay_Milliseconds (1);
   --  BMM150 power on reset time

   --  Power BMM150 on
   BMM150_SPI.Suspend_Off (Ravenscar_Time.Delays, Ok);
   pragma Assert (Ok);

   --  Look for BMM150 chip
   if not BMM150_SPI.Check_Chip_Id then
      Ada.Text_IO.Put_Line ("BMM150 not found.");
      raise Program_Error;
   end if;

   --  Reset BMM150
   BMM150_SPI.Reset (Ok);
   pragma Assert (Ok);

   --  Read calibration data into Trim
   BMM150_SPI.Read_Trim_Registers (Trim, Ok);

   --  Set the number of repetitions on X/Y and Z axes
   BMM150_SPI.Set_Repetitions
     (Preset  => BMM150.Low_Power_Preset,
      Success => Ok);
   pragma Assert (Ok);

   --  Enable contigous measurement mode
   BMM150_SPI.Set_Power_Mode
     (Mode    => BMM150.Normal,
      Success => Ok);
   pragma Assert (Ok);

   loop
      STM32.Board.Toggle (STM32.Board.D1_LED);

      for J in Vector'Range loop

         Ravenscar_Time.Delays.Delay_Milliseconds (100);

         --  Read scaled values from the sensor
         BMM150_SPI.Read_Raw_Measurement (Vector (J), R_Hall, Ok);
         pragma Assert (Ok);
      end loop;

      --  Printing...
      declare
      begin
         Ada.Text_IO.New_Line;
         Ada.Text_IO.New_Line;

         for Value of Vector loop
            declare
               X : constant String := Value.X'Image;
               Y : constant String := Value.Y'Image;
               Z : constant String := Value.Z'Image;
            begin
               Ada.Text_IO.Put_Line (X & ", " & Y & ", " & Z);
            end;
         end loop;

         Ada.Text_IO.Put_Line ("Sleeping 0.5s...");
         Ravenscar_Time.Delays.Delay_Milliseconds (500);
      end;
   end loop;
end Main;
