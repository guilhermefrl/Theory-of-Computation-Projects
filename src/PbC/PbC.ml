open Char
open List
open Str
open Scanf
open Printf

type regexp =
 | V  
 | E
 | C of char
 | U of regexp * regexp 
 | P of regexp * regexp 
 | S of regexp

 type simbolo = char option (* Conjunto de simbolos que provocam uma transição*)
 type fita = simbolo list (*Lista de simbolos de entrada *)
 type estado = int   (*Estado, representado  *)
 type transicao = (estado * simbolo) * estado (*Transicao estado->simbolo->estado *)
 type memoria = estado list * fita 
 type maquina = (transicao list * estado list * estado list)
 type automato = transicao list * estado list * estado list

 exception FIM of memoria

module Parser_regexp = struct

  
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | RPAREN
    | LPAREN
    | EPS
    | EOF
    | EMP
    | CONC
    | CHAR of (
       (char)
  )
    | AST
    | ALT
  
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
  | MenhirState12
  | MenhirState7
  | MenhirState6
  | MenhirState1
  | MenhirState0


let rec _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv47 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv45 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_term)), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
                                ( P (e1, e2) )
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv46)) : 'freshtv48)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv51 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv49 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_term)), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
                                ( U (e1, e2) )
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv50)) : 'freshtv52)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv55 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv53 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_atom = 
                                ( e )
             in
            _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv54)) : 'freshtv56)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv57 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)) : 'freshtv60)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv69 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv67 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (le : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : (
       (regexp)
            ) = 
                                ( le )
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv65) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
       (regexp)
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv63) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
       (regexp)
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv61) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
       (regexp)
            )) : (
       (regexp)
            )) = _v in
            (Obj.magic _1 : 'freshtv62)) : 'freshtv64)) : 'freshtv66)) : 'freshtv68)) : 'freshtv70)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv71 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)) : 'freshtv74)
    | _ ->
        let (() : unit) = () in
        ((Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
        assert false) : 'freshtv75)

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState12 | MenhirState6 | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ALT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv31 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CHAR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | EMP ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | EPS ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | LPAREN ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv32)
        | CONC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv33 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CHAR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
            | EMP ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | EPS ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | LPAREN ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv34)
        | EOF | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (e : 'tv_term)) = _menhir_stack in
            let _v : 'tv_expr = 
                                ( e )
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv36)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv37 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)) : 'freshtv40)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * 'tv_factor) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * 'tv_factor) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_factor)), _, (e2 : 'tv_term)) = _menhir_stack in
        let _v : 'tv_term = 
                                ( P (e1, e2) )
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv42)) : 'freshtv44)

and _menhir_goto_factor : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_factor -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv29 * _menhir_state * 'tv_factor) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CHAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | EMP ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | EPS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LPAREN ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | ALT | CONC | EOF | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state * 'tv_factor) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (e : 'tv_factor)) = _menhir_stack in
        let _v : 'tv_term = 
                                ( e )
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv28)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv30)

and _menhir_goto_atom : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AST ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (e : 'tv_atom)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_factor = 
                                ( S e )
         in
        _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v) : 'freshtv20)) : 'freshtv22)
    | ALT | CHAR _ | CONC | EMP | EOF | EPS | LPAREN | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (e : 'tv_atom)) = _menhir_stack in
        let _v : 'tv_factor = 
                                ( e )
         in
        _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v) : 'freshtv24)) : 'freshtv26)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv9 * _menhir_state * 'tv_term)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state * 'tv_factor) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv13 * _menhir_state * 'tv_term)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv18)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CHAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | EMP ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | EPS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LPAREN ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_atom = 
                                ( E )
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv8)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_atom = 
                                ( V )
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
       (char)
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
       (char)
    )) : (
       (char)
    )) = _v in
    ((let _v : 'tv_atom = 
                                ( C c )
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)

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

and regexpr : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
       (regexp)
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CHAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | EMP ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EPS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

  



end

module Lexer_regexp = struct
 
  open Parser_regexp

  exception Error of string


  let __ocaml_lex_tables = {
    Lexing.lex_base =
    "\000\000\245\255\246\255\247\255\248\255\249\255\250\255\251\255\
      \252\255\253\255\254\255\255\255";
    Lexing.lex_backtrk =
    "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255";
    Lexing.lex_default =
    "\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000";
    Lexing.lex_trans =
    "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\011\000\011\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \004\000\003\000\007\000\009\000\000\000\000\000\008\000\000\000\
      \006\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \002\000";
    Lexing.lex_check =
    "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \000\000\000\000\000\000\000\000\255\255\255\255\000\000\255\255\
      \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
      \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \000\000";
    Lexing.lex_base_code =
    "";
    Lexing.lex_backtrk_code =
    "";
    Lexing.lex_default_code =
    "";
    Lexing.lex_trans_code =
    "";
    Lexing.lex_check_code =
    "";
    Lexing.lex_code =
    "";
  }

  let rec tokenize lexbuf =
    __ocaml_lex_tokenize_rec lexbuf 0
  and __ocaml_lex_tokenize_rec lexbuf __ocaml_lex_state =
    match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
        | 0 ->
                                        ( tokenize lexbuf )

    | 1 ->
  let
                          s
  = Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
                                        ( CHAR s )

    | 2 ->
                                        ( ALT )

    | 3 ->
                                        ( CONC )

    | 4 ->
                                        ( AST )

    | 5 ->
                                        ( EMP )

    | 6 ->
                                        ( EPS )

    | 7 ->
                                        ( LPAREN )

    | 8 ->
                                        ( RPAREN )

    | 9 ->
                                        ( EOF )

    | 10 ->
        ( raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) )

    | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
        __ocaml_lex_tokenize_rec lexbuf __ocaml_lex_state

  ;;
end

(* --------------------------------- fim lexing/parsing code ----------------------------------------------------- *)
open Parser_regexp

(* função principal de leitura de uma expressÃ£o regular (a partir de uma string) *)
let regexp st =
  let linebuf = Lexing.from_string st in
  try regexpr Lexer_regexp.tokenize linebuf
  with _ -> failwith "regexp: input problem"


(* **************************************************************************************************************** *)
(* ********************************************   Começar aqui **************************************************** *)

let rec string_of_regexp s =
  match s with
  | V       -> "0"
  | E       -> "1"
  | C  c    -> String.make 1 c    
  | U (f,g) -> "("^(string_of_regexp f)^" + "^(string_of_regexp g)^")"
  | P (f,g) -> "("^(string_of_regexp f)^" . "^(string_of_regexp g)^")"
  | S s     -> (string_of_regexp s)^"*"

  let to_fita c =  if c='_' then None else (Some c)(*Funcao que traduz de char para simbolo reconhecido pelo automato*)

    let em_par a b c =  ((a, to_fita b),c) (*Funcao que transforma um char b e dois int a e c no tipo transição*)

      let rec to_automato e le lef = (*Funcao que transforma uma expressao regular num conjunto de transicoes*)
       let lista = ref [] in (*Cria a lista vazia que vai ser retornada com o conjunto de transições*)
      match e with 
        | V ->   lista :=  (em_par le '_' lef ) :: !lista;(!lista,le,lef)
        | E ->   lista :=  (em_par le '_' lef ) :: !lista; (!lista,le,lef)
        | C c ->  lista :=  (em_par le c lef ) :: !lista; (!lista,le,lef) (*Quando encontrar um char, então realiza uma transição de le para lef com o char c e acrescenta a lista, retornando a lista das transições, juntamente com o estado inicial  e final*)
        | U (f,g) -> (aux1 f g le lef) (*Quando encontra uma União é chamada a função aux1, que resolve os casos de união e retorna uma lista com as transições correspondentes e o estado inicial e final*)
        | P (f,g) -> (aux2 f g le lef)(*Quando encontra uma concatenação é chamada a função aux2, que resolve os casos de concatenação e retorna uma lista com as transições correspondentes e o estado inicial e final*)
        | S s     ->  (aux3 s le lef) (*Quando encontra uma asterisco é chamada a função aux3, que resolve os casos do asterisco e retorna uma lista com as transições correspondentes e o estado inicial e final*)
    
      and aux1 f g le lef = (*Função usada para os casos de união*)
        let l1= ref [] in
        let l2= ref [] in
     
        l1 := (em_par le '_' lef) :: !l1; (*Começamos por juntar uma transição epsilon de le para lef, ou seja para criar o primeiro ramo da União*)
        let ll1,le1,lef1 = (to_automato (f) (le+1) (lef+1)) in (*De seguida chamamos a função to_automato para a primeira parte da União, chamando para le+1 e lef+1, assim permite que a transição anterior feita para acabar em lef, começe aqui neste ponto*)
        l1 := ll1 @ !l1; (*Juntamos a lista que retorna a primeira parte da União, com a transição epsilon feita em primeiro lugar*)
      
        l2 :=  (em_par (le) ('_') (lef1+1) ) :: !l2; (*Aqui realizamos uma transição epsilon que permite realizar a segunda parte da união, começando no estado inicial da primeira parte, para o estado final da primeira parte +1*)
        let ll2,le2,lef2=(to_automato (g) (lef1+1) (lef1+2))  in (*Vamos chamar a função  to_automato para a segunda parte da união, agora começando em lef1+1, o que permite realizar a segunda parte partindo da transição epsilon feita para este efeito*)
      
        l2 := ll2 @ !l2; (*Juntamos em l2 a lista da transição epsilon com a lista que contêm a segunda parte da união*)
          l2 :=  (em_par (lef1) ('_') (lef2+1) ) :: !l2; (*Estas duas transições epsilon servem para fechar as duas partes da união, permitindo assim que as duas partes separadas possam ter o mesmo estado final, sendo assim possível realizar uma parte da união ou a outra*)
          l2 :=  (em_par (lef2) ('_') (lef2+1) ) :: !l2;

        let l3 = List.append !l1 !l2 in (*Juntamos em l3 todas as listas anteriores e retornamos a mesma*)
      (l3,le2,lef2+1) (*retornamos l3 e o estado inicial que é le2, e também lef2+1 porque é o estado onde acabam as duas partes da união*)

      and aux2 f g le lef = (*Função que trata dos casos em que encontra uma concatenação, sendo bastante simples a mesma*)
       let l1,le1,lef1 = (to_automato(f)(le)(lef)) in (*Primeira parte sendo resolvida com esta call para a primeira parte da concatenação *)
     let l2,le2,lef2 = (to_automato(g)(lef1)(lef1+1)) in (*Aqui chamamos para a segunda parte da concatenação, simplesmente usando o estado final do anterior como estado inicial e o estado final+1 como estado inicial*)
      let l3 = List.append l1 l2 in (*Jutamos as listas das duas partes da concatenação e retornamos a mesma na linha seguinte, também o estado inicial e final*)
      (l3,le2,lef2)

      and aux3 s le lef = (*Função para tratar os casos do * *)
      let l1= ref [] in
       let ll1,le1,lef1 = (to_automato(s)(le)(lef)) in (*Chamamos a função to_automato para resolver todas as situações que possam existir dentro do conteúdo do mesmo*)
      
      l1 := (em_par (lef1) ('_') (le) ) :: !l1; (*Realizamos uma transição de lef1 para le e permite andar para trás de tudo o que está contido no *, ou seja permite realizar o que esta dentro do * as vezes que forem necessárias*)
      l1 :=  (em_par (le) ('_') (lef1)) :: !l1; (*Esta transição aqui adicionada a l1 permite saltar o *, ou seja realizar 0 vezes o seu conteudo*)
      l1 := ll1 @ !l1; (*Jutamos a lista que foi retornada do to_automato com a lista a que adicionamos as transições epsilon*)
      (!l1,le1,lef1)  (*Retornamos a lista final, e os estados iniciais e finais*)


      let char_of_string s = s.[0]
      
      let leitura dad ini fin inp = (*Função de leitura adaptada, que permite a passagem dos vários parâmetros e o seu retorno como dados e tipo automato*)
        let dados = List.map (fun x -> to_fita (char_of_string x)) 
          (Str.split (Str.regexp "[ \t]+") (dad)) in
        let initl =  List.map (fun x -> int_of_string x) 
          (Str.split (Str.regexp "[ \t]+")  (ini)) in
        let finl = List.map (fun x -> int_of_string x) 
          (Str.split (Str.regexp "[ \t]+")  (fin))  in
        let input = ref inp in(*O input é diretamente igual ao inp passado como parâmetro pois o mesmo já é uma lista de transições*)
             try
           (dados,(!input,initl,finl)) 
        with _ -> (dados,(!input,initl,finl))


     let subset c1 c2 = 
      for_all  (function x -> (mem x c2)) c1;;
    
    let  equal c1 c2 = 
      (subset c1 c2) && (subset c2 c1);;
     
  let normalize c = 
    fold_left 
      (fun res x -> 
        if (mem x res) 
        then res 
        else (x::res) ) 
      [] c

     let union c1 c2 = 
      normalize  (c1@c2);;

     let epsilon_trans_aux state maq = 
      let transicoes,b,c = maq in
      map (function (a,b) -> b) 
        (filter (function (x,y) -> x=(state,None)) transicoes)

     let rec epsilon_trans lstate maq =   
        let res = 
          (normalize (flatten (map (fun x -> epsilon_trans_aux x maq) lstate))) in
      
        let resultado = (union res lstate) in
        if (equal lstate resultado) 
        then resultado  
        else (epsilon_trans resultado  maq) 

        let  select est simb tabela =
          map (function (a,b) -> b) (filter (function  ((a,b),c) -> a=est && b=simb) tabela)

        let next simb maquina memo =   
          let (lesta, restante) = memo in
          let (transicoes,b,c) = maquina in    
          let tr = (fold_left 
                      (fun x y -> (select y simb transicoes)@x ) 
                      [] lesta)
          in    
          let res =  epsilon_trans  (normalize tr) maquina  in
          if (res = []) then raise (FIM memo) else res

     let step memo maq = 
      let (laqui, restante) = memo in
      match restante with
          [] ->  raise (FIM memo)
        | el::li ->  (next el maq memo,li)

    let is_accepted memo maq =
      let (laqui, restante) = memo in
      let (trans,init,accept)= maq in
      restante=[]&& (exists (fun x -> mem x accept) laqui)
    
     let print_output memo maq= 
      if (is_accepted memo maq) 
      then printf "YES\n"
      else printf"NO\n"

    let est_fin s =
      let c = ref 0 in
      let sl = String.length s in

      for i=0 to sl-1 do
        if(s.[i] = '.')
        then 
          c := !c + 1;
      done;
    (!c)
    
   let () =
    let ex1 = read_line() in (*Lemos a expressão regular introduzida, e guardamos na variavel ex1*)
    let ex2 = String.concat ex1 ["(A+C+T+G)*("; ")(A+C+T+G)*"] in (*Adicionamos a expressão regular  (A+C+T+G)* no inicio e no fim a mesma expressão, isto não muda nada na expressão introduzida pelo utilizador, mas permite que o automato faça a busca por qualquer sub-string da string introduzida na linha seguinte, pois (A+C+T+G)* permite realizar estes caracteres todas as vezes necessárias e permite acabar quando for encontrando uma sub string que corresponde á expressão introduzida pelo utilizador, funciona de igual modo quando introduzida no fim, pois permite que seja totalmente processada a parte final da string, chegando assim ao estado final pretendido *)
    let ex = regexp (ex2) in (*Passamos a ex2, já com a parte (A+C+T+G)* introduzida no inicio e no fim*)
    let r = read_line() in (*Lemos a string que queremos verificar se corresponde a expressão regular introduzida*)
    let aux1,ab,bc= to_automato ex 0 1 in (*Chamamos a função to_automato para a expressão regular introduzida, mais a parte depois acrescentada*)
  
    let i = ref 0 in
    let z1 = ref "" in
  
    while !i < String.length r do
      z1 := String.concat !z1 [""; Char.escaped (r.[!i])];
      z1 := String.concat !z1 [""; " "];
        
      i := !i + 1;
    done; (*Este while serve para adicionar espaços entre todos os elementos da string, isto permite corresponder ao modelo de entrada da função leitura do modulo FSA*)

    let fin = ref 0 in
      fin := bc; 
      let fin2 = string_of_int !fin in (*O estado final é igual ao valor retornado pela função to_automato, e convertemos esse valor para string*)

    let dad = !z1 in
    let ini = "0" in
    let inp = aux1 in  
    let fin1 = fin2 in 

    let dados,maquina = (leitura (dad) (ini) (fin1) (inp)) in (*Chamamos a função leitura do modulo FSA para os dados introduzidos, o estado inical,o estado final e a lista das transições*)
    let (a,b,c) = maquina in (* Apartir daqui é utilizado o modulo FSA, presente no site do Professor e sendo este responsável pelo processamento da string e do autómato *)
    try
    let memor = ref ((epsilon_trans b maquina),dados)  in
    while true do
      memor := (step !memor maquina)
    done
  with
      FIM x -> print_output x maquina;

(*Ideia Base retirada do site: 
https://www.di.ubi.pt/~desousa/TC/FSA/Fsa.html
https://www.youtube.com/watch?v=RYNN-tb9WxI&t=275s
*)

(*
Exemplo:

(TAG+TC)(A+C+G+T)*TGC
ATTGCAGTAGGACTCGCCTGATGCAGTC

1º - Juntar (A+C+T+G)* ao início e ao fim da expressão regular, isto permite processar toda a string, e encontrar a parte que corresponde ao automato sendo também adicionados parênteses à expressão regular.

(A+C+T+G)*((TAG+TC)(A+C+G+T)*TGC)(A+C+T+G)*


2º - Fazer todas as transições do autómato, apartir da expressão regular, sendo feitas pela função to_automato. (As transições "_" representam as transições epsilon)

0 _ 1  1 A 2  0 _ 3  3 _ 4  4 C 5  3 _ 6  6 _ 7  7 T 8  6 _ 9  9 G 10  10 _ 11  8 _ 11  11 _ 12  5 _ 12  12 _ 13  2 _ 13  0 _ 13  13 _ 0  (Transições do (A+C+T+G)* )

13 _ 14  14 T 15  15 A 16  16 G 17  17  _ 21  13 _ 18  18 T 19  19 C 20  20 _ 21  (Transições do (TAG+TC) )

21 _ 22  22 A 23  23 _ 34  21 _ 24  24 _ 25  25 C 26  26 _ 33  24 _ 27  27 _ 28  28 G 29  29 _ 32  27 _ 30  30 T 31  31 _ 32  32 _ 33  33 _ 34  21 _ 34  34 _ 21  (Transições do (A+C+G+T)* )
                      
34 T 35  35 G 36  36 C 37  (Transições do TGC )

37 _ 38  38 A 39  39 _ 50  37 _ 40  40 _ 41  41 C 42  42 _ 49  40 _ 43  43 _ 44  44 T 45  45 _ 48  43 _ 46  46 G 47  47 _ 48  48 _ 49  49 _ 50  37 _ 50  50 _ 37  (Transições do (A+C+G+T)* )

(No desenho o "ε" significa as transições vazias)

Link do desenho do autómato: https://ubipt-my.sharepoint.com/:i:/g/personal/guilherme_lacao_ubi_pt/EQtQ5TBuvxlOl2AeQYlSI8cBDIB5xHTRzh_OwEdKxc40Rg?e=0EdUTb

3º - Inserir a string, o estado inicial e final do autómato, e as transições do mesmo, na função leitura, sendo que o autómato é executado apartir do módulo FSA, presente no site do Professor, e a função 'print_output' retorna o output. 
4º - Neste caso o Output é YES pois existe uma parte da string que corresponde ao automato introduzido, sendo esta "TAGGACTCGCCTGATGC".

Output: YES

*)