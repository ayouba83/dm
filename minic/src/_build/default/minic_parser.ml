
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | XOR
    | WHILE
    | VOID
    | SUB
    | SET
    | SEMI
    | RPAR
    | RETURN
    | PUTCHAR
    | PLUS
    | ORL
    | OR
    | NOT
    | NE
    | MUL
    | LT
    | LSL
    | LPAR
    | LE
    | INT
    | INCR
    | IF
    | IDENT of (
# 13 "minic_parser.mly"
       (string)
# 33 "minic_parser.ml"
  )
    | GT
    | GE
    | EQ
    | EOF
    | END
    | ELSE
    | DIV
    | DECR
    | CST of (
# 11 "minic_parser.mly"
       (int)
# 46 "minic_parser.ml"
  )
    | COMA
    | BOOL_CST of (
# 12 "minic_parser.mly"
       (bool)
# 52 "minic_parser.ml"
  )
    | BOOL
    | BEGIN
    | ASR
    | ANDL
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState104
  | MenhirState102
  | MenhirState93
  | MenhirState90
  | MenhirState84
  | MenhirState82
  | MenhirState79
  | MenhirState74
  | MenhirState70
  | MenhirState69
  | MenhirState57
  | MenhirState55
  | MenhirState53
  | MenhirState51
  | MenhirState49
  | MenhirState47
  | MenhirState45
  | MenhirState43
  | MenhirState41
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState29
  | MenhirState27
  | MenhirState26
  | MenhirState25
  | MenhirState24
  | MenhirState22
  | MenhirState20
  | MenhirState16
  | MenhirState11
  | MenhirState5
  | MenhirState0

# 1 "minic_parser.mly"
  

  open Lexing
  open Minic_ast


# 117 "minic_parser.ml"

let rec _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | IDENT _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | IF ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | PUTCHAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | RETURN ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | WHILE ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | END ->
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_goto_args_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (vd : (Minic_ast.expr))), _, (dl : (Minic_ast.expr list))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.expr list) = 
# 172 "minic_parser.mly"
                                  ( vd :: dl )
# 165 "minic_parser.ml"
         in
        _menhir_goto_args_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (f : (
# 13 "minic_parser.mly"
       (string)
# 180 "minic_parser.ml"
            ))), _, (params : (Minic_ast.expr list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 164 "minic_parser.mly"
                                     ( Call(f, params) )
# 187 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.expr list) = 
# 170 "minic_parser.mly"
              ( [] )
# 446 "minic_parser.ml"
     in
    _menhir_goto_args_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ELSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BOOL_CST _v ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
                    | CST _v ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
                    | IDENT _v ->
                        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
                    | IF ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState90
                    | LPAR ->
                        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState90
                    | NOT ->
                        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState90
                    | PUTCHAR ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState90
                    | RETURN ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState90
                    | SUB ->
                        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState90
                    | WHILE ->
                        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState90
                    | END ->
                        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState90
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))), _, (s1 : (Minic_ast.seq))), _, (s2 : (Minic_ast.seq))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 133 "minic_parser.mly"
                                                                                               ( If(e, s1, s2) )
# 561 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Minic_ast.instr))), _, (xs : (Minic_ast.seq))) = _menhir_stack in
        let _v : (Minic_ast.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 577 "minic_parser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 134 "minic_parser.mly"
                                                             ( While(e, s) )
# 598 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (f : (
# 13 "minic_parser.mly"
       (string)
# 619 "minic_parser.ml"
            ))), _, (opt : ((string * Minic_ast.typ) list))), _, (vars : ((string * Minic_ast.typ) list))), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _9 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (Minic_ast.fun_def) = 
# 124 "minic_parser.mly"
    ( { name=f; params=opt; return=t; locals=vars; code=s } )
# 628 "minic_parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | INT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | EOF ->
                _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce25 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 13 "minic_parser.mly"
       (string)
# 659 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (n : (
# 13 "minic_parser.mly"
       (string)
# 665 "minic_parser.ml"
    ))) = _menhir_stack in
    let _v : (Minic_ast.expr) = 
# 163 "minic_parser.mly"
            ( Get(n) )
# 670 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "minic_parser.mly"
       (string)
# 677 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DECR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
    | INCR ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
    | LPAR ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
    | ANDL | COMA | DIV | EQ | GE | GT | LE | LT | MUL | NE | ORL | PLUS | RPAR | SEMI | SUB ->
        _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 13 "minic_parser.mly"
       (string)
# 702 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | RPAR ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 13 "minic_parser.mly"
       (string)
# 730 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (id : (
# 13 "minic_parser.mly"
       (string)
# 738 "minic_parser.ml"
    ))) = _menhir_stack in
    let _2 = () in
    let _v : (Minic_ast.expr) = 
# 161 "minic_parser.mly"
                ( Incr( id ) )
# 744 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 13 "minic_parser.mly"
       (string)
# 751 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (id : (
# 13 "minic_parser.mly"
       (string)
# 759 "minic_parser.ml"
    ))) = _menhir_stack in
    let _2 = () in
    let _v : (Minic_ast.expr) = 
# 162 "minic_parser.mly"
                ( Decr( id ) )
# 765 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState57 | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ANDL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | COMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL_CST _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | CST _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | IDENT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | LPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | NOT ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | SUB ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | RPAR ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Minic_ast.expr))) = _menhir_stack in
            let _v : (Minic_ast.expr list) = 
# 171 "minic_parser.mly"
               ( [e] )
# 831 "minic_parser.ml"
             in
            _menhir_goto_args_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | ANDL | COMA | EQ | GE | GT | LE | LT | NE | ORL | PLUS | RPAR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 150 "minic_parser.mly"
                                 ( Sub(a, b) )
# 856 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.expr) = 
# 147 "minic_parser.mly"
                                 ( Mul(a, b) )
# 873 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.expr) = 
# 149 "minic_parser.mly"
                                 ( Div(a, b) )
# 884 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | ANDL | COMA | EQ | GE | GT | LE | LT | NE | ORL | PLUS | RPAR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 148 "minic_parser.mly"
                                 ( Add(a, b) )
# 903 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ANDL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | COMA | ORL | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 160 "minic_parser.mly"
                                 ( Orl(a, b) )
# 946 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | ANDL | COMA | EQ | NE | ORL | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 155 "minic_parser.mly"
                                 ( Ne(a, b) )
# 983 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | ANDL | COMA | EQ | GE | GT | LE | LT | NE | ORL | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 152 "minic_parser.mly"
                                 ( Lt(a, b) )
# 1012 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | ANDL | COMA | EQ | GE | GT | LE | LT | NE | ORL | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 156 "minic_parser.mly"
                                 ( Le(a, b) )
# 1041 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | ANDL | COMA | EQ | GE | GT | LE | LT | NE | ORL | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 153 "minic_parser.mly"
                                 ( Gt(a, b) )
# 1070 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | ANDL | COMA | EQ | GE | GT | LE | LT | NE | ORL | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 157 "minic_parser.mly"
                                 ( Ge(a, b) )
# 1099 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | ANDL | COMA | EQ | NE | ORL | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 154 "minic_parser.mly"
                                 ( Eq(a, b) )
# 1136 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | ANDL | COMA | ORL | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 159 "minic_parser.mly"
                                ( Andl(a, b) )
# 1177 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ANDL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.expr) = 
# 165 "minic_parser.mly"
                         ( Par(e) )
# 1223 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (b : (Minic_ast.expr))) = _menhir_stack in
        let _1 = () in
        let _v : (Minic_ast.expr) = 
# 158 "minic_parser.mly"
                    ( Not(b) )
# 1242 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (a : (Minic_ast.expr))) = _menhir_stack in
        let _1 = () in
        let _v : (Minic_ast.expr) = 
# 151 "minic_parser.mly"
                                ( Opp(a) )
# 1253 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ANDL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL_CST _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
                | CST _v ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
                | IDENT _v ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
                | IF ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState69
                | LPAR ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState69
                | NOT ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState69
                | PUTCHAR ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState69
                | RETURN ->
                    _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69
                | SUB ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState69
                | WHILE ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState69
                | END ->
                    _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState69
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ANDL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 135 "minic_parser.mly"
                           ( Return(e) )
# 1370 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ANDL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 131 "minic_parser.mly"
                                      ( Putchar(e) )
# 1425 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ANDL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL_CST _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
                | CST _v ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
                | IDENT _v ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
                | IF ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | LPAR ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | NOT ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | PUTCHAR ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | RETURN ->
                    _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | SUB ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | WHILE ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | END ->
                    _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ANDL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (id : (
# 13 "minic_parser.mly"
       (string)
# 1553 "minic_parser.ml"
            ))), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Minic_ast.instr) = 
# 132 "minic_parser.mly"
                                 ( Set(id, e) )
# 1560 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 | MenhirState69 | MenhirState82 | MenhirState90 | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ANDL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.instr) = 
# 136 "minic_parser.mly"
                    ( Expr(e) )
# 1607 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 1626 "minic_parser.ml"
     in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
        | CST _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
        | IDENT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | NOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | CST _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | IDENT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | CST _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NOT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | SUB ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | CST _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | IDENT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | NOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "minic_parser.mly"
       (string)
# 1827 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DECR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
    | INCR ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
    | LPAR ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
    | SET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | CST _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | IDENT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | NOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | ANDL | DIV | EQ | GE | GT | LE | LT | MUL | NE | ORL | PLUS | SEMI | SUB ->
        _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "minic_parser.mly"
       (int)
# 1873 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 11 "minic_parser.mly"
       (int)
# 1881 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 145 "minic_parser.mly"
        ( Cst(n) )
# 1886 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "minic_parser.mly"
       (bool)
# 1893 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 12 "minic_parser.mly"
       (bool)
# 1901 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 146 "minic_parser.mly"
             ( BCst(b) )
# 1906 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_locale_dcl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Minic_ast.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Minic_ast.typ))), _, (xs : ((string * Minic_ast.typ) list))) = _menhir_stack in
        let _v : ((string * Minic_ast.typ) list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1921 "minic_parser.ml"
         in
        _menhir_goto_list_locale_dcl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | CST _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | IDENT _v ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | IF ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | LPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | NOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | PUTCHAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | RETURN ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | WHILE ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | END ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Minic_ast.typ) list) = 
# 211 "<standard.mly>"
    ( [] )
# 1968 "minic_parser.ml"
     in
    _menhir_goto_list_locale_dcl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_param_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Minic_ast.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState16
                | BOOL_CST _ | CST _ | END | IDENT _ | IF | LPAR | NOT | PUTCHAR | RETURN | SUB | WHILE ->
                    _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState16
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (vd : (string * Minic_ast.typ))), _, (dl : ((string * Minic_ast.typ) list))) = _menhir_stack in
        let _2 = () in
        let _v : ((string * Minic_ast.typ) list) = 
# 100 "minic_parser.mly"
                                  ( vd :: dl )
# 2023 "minic_parser.ml"
         in
        _menhir_goto_param_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Minic_ast.typ) list) = 
# 98 "minic_parser.mly"
              ( [] )
# 2034 "minic_parser.ml"
     in
    _menhir_goto_param_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 41 "minic_parser.mly"
      (Minic_ast.prog)
# 2041 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 41 "minic_parser.mly"
      (Minic_ast.prog)
# 2049 "minic_parser.ml"
    )) = _v in
    Obj.magic _1

and _menhir_goto_declaration_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Minic_ast.typ * int) list * Minic_ast.fun_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (fd : (Minic_ast.fun_def))), _, (dl : ((string * Minic_ast.typ * int) list * Minic_ast.fun_def list))) = _menhir_stack in
        let _v : ((string * Minic_ast.typ * int) list * Minic_ast.fun_def list) = 
# 78 "minic_parser.mly"
                                       ( let vl, fl = dl in
                                         vl, fd :: fl )
# 2065 "minic_parser.ml"
         in
        _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (vd : (string * Minic_ast.typ * int))), _, (dl : ((string * Minic_ast.typ * int) list * Minic_ast.fun_def list))) = _menhir_stack in
        let _v : ((string * Minic_ast.typ * int) list * Minic_ast.fun_def list) = 
# 76 "minic_parser.mly"
                                       ( let vl, fl = dl in
                                         vd :: vl, fl )
# 2076 "minic_parser.ml"
         in
        _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (dl : ((string * Minic_ast.typ * int) list * Minic_ast.fun_def list))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 41 "minic_parser.mly"
      (Minic_ast.prog)
# 2092 "minic_parser.ml"
            ) = 
# 62 "minic_parser.mly"
       ( let var_list, fun_list = dl in
         { globals = var_list; functions = fun_list; } )
# 2097 "minic_parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState104 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | RPAR ->
                    _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
            | SET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CST _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | SEMI ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (x : (
# 13 "minic_parser.mly"
       (string)
# 2159 "minic_parser.ml"
                        ))), (n : (
# 11 "minic_parser.mly"
       (int)
# 2163 "minic_parser.ml"
                        ))) = _menhir_stack in
                        let _5 = () in
                        let _3 = () in
                        let _v : (string * Minic_ast.typ * int) = 
# 88 "minic_parser.mly"
                               ( (x, t, n) )
# 2170 "minic_parser.ml"
                         in
                        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        assert (not _menhir_env._menhir_error);
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | BOOL ->
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                        | INT ->
                            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                        | VOID ->
                            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                        | EOF ->
                            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState102 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (
# 13 "minic_parser.mly"
       (string)
# 2225 "minic_parser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (t : (Minic_ast.typ))) = _menhir_stack in
            let _v : (string * Minic_ast.typ) = 
# 103 "minic_parser.mly"
               ( (x, t) )
# 2231 "minic_parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                | RPAR ->
                    _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (p : (string * Minic_ast.typ))) = _menhir_stack in
                let _v : ((string * Minic_ast.typ) list) = 
# 99 "minic_parser.mly"
              ( [p] )
# 2261 "minic_parser.ml"
                 in
                _menhir_goto_param_list _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (x : (
# 13 "minic_parser.mly"
       (string)
# 2294 "minic_parser.ml"
                ))) = _menhir_stack in
                let _3 = () in
                let _v : (string * Minic_ast.typ) = 
# 94 "minic_parser.mly"
                    ( (x, t) )
# 2300 "minic_parser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState20
                | BOOL_CST _ | CST _ | END | IDENT _ | IF | LPAR | NOT | PUTCHAR | RETURN | SUB | WHILE ->
                    _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState20
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos__1_ = _startpos in
        let _1 = () in
        let _v : (
# 41 "minic_parser.mly"
      (Minic_ast.prog)
# 2480 "minic_parser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 64 "minic_parser.mly"
        ( let pos = _startpos in
          let message =
            Printf.sprintf
              "Syntax error at %d, %d"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message )
# 2491 "minic_parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Minic_ast.typ * int) list * Minic_ast.fun_def list) = 
# 75 "minic_parser.mly"
             ( [], [] )
# 2500 "minic_parser.ml"
     in
    _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 113 "minic_parser.mly"
       ( Void )
# 2512 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 111 "minic_parser.mly"
      ( Int )
# 2524 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 112 "minic_parser.mly"
       ( Bool )
# 2536 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 41 "minic_parser.mly"
      (Minic_ast.prog)
# 2555 "minic_parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 2585 "minic_parser.ml"
