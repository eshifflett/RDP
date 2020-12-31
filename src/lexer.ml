open TokenTypes

let tokenize input =
  let length = String.length input in
    let rec tokenize_helper pos =
      if (pos >= length) then
        [EOF]
      else if (Str.string_match (Str.regexp("(")) input pos) then
        Tok_LParen :: (tokenize_helper (pos + 1))
      else if (Str.string_match (Str.regexp(")")) input pos) then
        Tok_RParen :: (tokenize_helper (pos + 1))
      else if (Str.string_match (Str.regexp("{")) input pos) then
        Tok_LBrace :: (tokenize_helper (pos + 1))
      else if (Str.string_match (Str.regexp("}")) input pos) then
        Tok_RBrace :: (tokenize_helper (pos + 1))
      else if (Str.string_match (Str.regexp("==")) input pos) then
        Tok_Equal :: (tokenize_helper (pos + 2))
      else if (Str.string_match (Str.regexp("!=")) input pos) then
        Tok_NotEqual :: (tokenize_helper (pos + 2))
      else if (Str.string_match (Str.regexp("=")) input pos) then
        Tok_Assign :: (tokenize_helper (pos + 1))
      else if (Str.string_match (Str.regexp(">=")) input pos) then
        Tok_GreaterEqual :: (tokenize_helper (pos + 2))
      else if (Str.string_match (Str.regexp("<=")) input pos) then
        Tok_LessEqual :: (tokenize_helper (pos + 2))  
      else if (Str.string_match (Str.regexp(">")) input pos) then
        Tok_Greater :: (tokenize_helper (pos + 1))
      else if (Str.string_match (Str.regexp("<")) input pos) then
        Tok_Less :: (tokenize_helper (pos + 1))
      else if (Str.string_match (Str.regexp("||")) input pos) then
        Tok_Or :: (tokenize_helper (pos + 2))
      else if (Str.string_match (Str.regexp("&&")) input pos) then
        Tok_And :: (tokenize_helper (pos + 2))
      else if (Str.string_match (Str.regexp("!")) input pos) then
        Tok_Not :: (tokenize_helper (pos + 1))
      else if (Str.string_match (Str.regexp(";")) input pos) then
        Tok_Semi :: (tokenize_helper (pos + 1))
      else if (Str.string_match (Str.regexp("\\+")) input pos) then
        Tok_Add :: (tokenize_helper (pos + 1))
      else if (Str.string_match (Str.regexp("\\*")) input pos) then
        Tok_Mult :: (tokenize_helper (pos + 1))
      else if (Str.string_match (Str.regexp("/")) input pos) then
        Tok_Div :: (tokenize_helper (pos + 1))
      else if (Str.string_match (Str.regexp("\\^")) input pos) then
        Tok_Pow :: (tokenize_helper (pos + 1))
      else if (Str.string_match (Str.regexp("[a-zA-Z][a-zA-Z0-9]*")) input pos) then
        let matched_str = Str.matched_string input in
          if (matched_str = "int") then Tok_Int_Type :: (tokenize_helper (pos + 3))
          else if (matched_str = "bool") then Tok_Bool_Type :: (tokenize_helper (pos + 4))
          else if (matched_str = "printf") then Tok_Print :: (tokenize_helper (pos + 6))
          else if (matched_str = "main") then Tok_Main :: (tokenize_helper (pos + 4))
          else if (matched_str = "if") then Tok_If :: (tokenize_helper (pos + 2))
          else if (matched_str = "else") then Tok_Else :: (tokenize_helper (pos + 4))
          else if (matched_str = "from") then Tok_From :: (tokenize_helper (pos + 4))
          else if (matched_str = "for") then Tok_For :: (tokenize_helper (pos + 3))
          else if (matched_str = "to") then Tok_To :: (tokenize_helper (pos + 2))
          else if (matched_str = "while") then Tok_While :: (tokenize_helper (pos + 5))
          else if (matched_str = "true") then (Tok_Bool true) :: (tokenize_helper (pos + 4))
          else if (matched_str = "false") then (Tok_Bool false) :: (tokenize_helper (pos + 5))
          else Tok_ID (matched_str) :: (tokenize_helper (pos + (String.length matched_str)))
      else if (Str.string_match (Str.regexp("-?[0-9]+")) input pos) then
        let matched_int = Str.matched_string input in
          Tok_Int (int_of_string matched_int) :: (tokenize_helper (pos + (String.length matched_int)))
      else if (Str.string_match (Str.regexp("-")) input pos) then
        Tok_Sub :: (tokenize_helper (pos + 1))
      else tokenize_helper (pos + 1)
    in tokenize_helper 0

