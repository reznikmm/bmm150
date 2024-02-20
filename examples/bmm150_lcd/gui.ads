--  SPDX-FileCopyrightText: 2024 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

pragma Ada_2022;

with GUI_Buttons;
with HAL.Bitmap;
with HAL.Touch_Panel;

package GUI is

   type Button_Kind is
     (Fx, Fy, Fz,       --  Field components
      LP, RE, EN, HA,   --  Low-power, regular, enhanced, high accuracy preset
      F2, F6, F8, F10, F15, F20, F25, F30);  --  Output rate

   function "+" (X : Button_Kind) return Natural is (Button_Kind'Pos (X))
     with Static;

   Buttons : constant GUI_Buttons.Button_Info_Array :=
     [(Label  => "Fx",
       Center => (23 * 1, 20),
       Color  => HAL.Bitmap.Red),
      (Label  => "Fy",
       Center => (23 * 2, 20),
       Color  => HAL.Bitmap.Green),
      (Label  => "Fz",
       Center => (23 * 3, 20),
       Color  => HAL.Bitmap.Blue),
      (Label  => "LP",
       Center => (23 * 1 + 160, 20),
       Color  => HAL.Bitmap.Yellow),
      (Label  => "RE",
       Center => (23 * 2 + 160, 20),
       Color  => HAL.Bitmap.Yellow),
      (Label  => "EN",
       Center => (23 * 3 + 160, 20),
       Color  => HAL.Bitmap.Yellow),
      (Label  => "HA",
       Center => (23 * 4 + 160, 20),
       Color  => HAL.Bitmap.Yellow),
      (Label  => "02",
       Center => (23 * 1 + 110, 220),
       Color  => HAL.Bitmap.Dim_Grey),
      (Label  => "06",
       Center => (23 * 2 + 110, 220),
       Color  => HAL.Bitmap.Dim_Grey),
      (Label  => "08",
       Center => (23 * 3 + 110, 220),
       Color  => HAL.Bitmap.Dim_Grey),
      (Label  => "10",
       Center => (23 * 4 + 110, 220),
       Color  => HAL.Bitmap.Dim_Grey),
      (Label  => "15",
       Center => (23 * 5 + 110, 220),
       Color  => HAL.Bitmap.Dim_Grey),
      (Label  => "20",
       Center => (23 * 6 + 110, 220),
       Color  => HAL.Bitmap.Dim_Grey),
      (Label  => "25",
       Center => (23 * 7 + 110, 220),
       Color  => HAL.Bitmap.Dim_Grey),
      (Label  => "30",
       Center => (23 * 8 + 110, 220),
       Color  => HAL.Bitmap.Dim_Grey)];

   State : GUI_Buttons.Boolean_Array (Buttons'Range) :=
     [+Fx | +Fy | +Fz | +LP | +F10 => True, others => False];

   procedure Check_Touch
     (TP     : in out HAL.Touch_Panel.Touch_Panel_Device'Class;
      Update : out Boolean);
   --  Check buttons touched, update State, set Update = True if State changed

   procedure Draw
     (LCD   : in out HAL.Bitmap.Bitmap_Buffer'Class;
      Clear : Boolean := False);

   procedure Dump_Screen (LCD : in out HAL.Bitmap.Bitmap_Buffer'Class);

end GUI;
