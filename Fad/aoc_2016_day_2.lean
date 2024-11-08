--  Definindo input
def input_day2 : String := include_str "day_2_2016.txt"

def split_by_new_line (s : String) : List String :=
  s.split (· == '\n')

def input_task := split_by_new_line input_day2

def join_numbers (lst : List Nat) : String :=
  String.intercalate "" (lst.map toString)

def teclado : List (List Nat) := [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
def teclado₁: List (List Char) := [['0', '0', '1', '0', '0'], ['0', '2', '3', '4', '0'], ['5','6','7','8','9'], ['0','A', 'B', 'C', '0'], [0, 0, 'D', 0, 0]]

def process_instructions (entrada : List String) : List Nat :=
  -- Começar do número 5 (ques está no centro do teclado)
  let start_posx := 1
  let start_posy := 1

  let (code, _, _) := entrada.foldl
    (fun (acc : List Nat × Nat × Nat) (linha : String) =>
      let (code, pos_x, pos_y) := acc

      let (final_pos_x, final_pos_y) := linha.toList.foldl
        (fun (pos : Nat × Nat) (i : Char) =>
          let (pos_x, pos_y) := pos
          if i = 'U' ∧ pos_y > 0 then (pos_x, pos_y - 1)
          else if i = 'D' ∧ pos_y < 2 then (pos_x, pos_y + 1)
          else if i = 'L' ∧ pos_x > 0 then (pos_x - 1, pos_y)
          else if i = 'R' ∧ pos_x < 2 then (pos_x + 1, pos_y)
          else (pos_x, pos_y)
        ) (pos_x, pos_y)
      (code ++ [teclado.get! final_pos_y |>.get! final_pos_x], final_pos_x, final_pos_y)
    ) ([], start_posx, start_posy)
  code

--  Duvida: como acessar
def process_instructions₁ (entrada : List String) : List Nat :=
  let start_posx := 0
  let start_posy := 2

   let (code, _, _) := entrada.foldl
    (fun (acc : List Nat × Nat × Nat) (linha : String) =>
      let (code, pos_x, pos_y) := acc

      let (final_pos_x, final_pos_y) := linha.toList.foldl
        (fun (pos : Nat × Nat) (i : Char) =>
          let (pos_x, pos_y) := pos
          if i = 'U' ∧ pos_y > 0 then (pos_x, pos_y - 1)
          else if i = 'D' ∧ pos_y < 5 ∧ (teclado₂.get! pos_y+1 |>.get! pos_x) !='0' then (pos_x, pos_y + 1)
          else if i = 'L' ∧ pos_x > 0 then (pos_x - 1, pos_y)
          else if i = 'R' ∧ pos_x < 5  then (pos_x + 1, pos_y)
          else (pos_x, pos_y)
        ) (pos_x, pos_y)
      (code ++ [teclado.get! final_pos_y |>.get! final_pos_x], final_pos_x, final_pos_y)
    ) ([], start_posx, start_posy)
  code




-- #eval process_instructions [['U', 'L', 'L'], ['R', 'R', 'D', 'D', 'D'], ['L', 'U', 'R', 'D', 'L'], ['U', 'U', 'U', 'U', 'D']]
-- #eval  join_numbers (process_instructions input_task)

def listas : List (List Nat) := [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
#eval  (listas.getD 1 []).getD 2 0 ≠ 6
