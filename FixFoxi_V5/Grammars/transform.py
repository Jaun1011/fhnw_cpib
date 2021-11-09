
from os import error
import re
from typing import Pattern, cast
import datetime

class Token:
    NTS = "NTS"
    COMENT = "COMENT"
    TS = "TS"
    ASSIGN = "ASSIGN"
    SEMICOLON = "SEMICOLON"
    OR = "OR"
    LNBRACKET = "LNBRACKET"
    RNBRACKET = "RNBRACKET"
    LBRACKET = "LBRACKET"
    RBRACKET = "RBRACKET"
    LEBRACKET = "LEBRACKET"
    REBRACKET = "REBRACKET"
    RANGE = "RANGE"
    QUOTE = 8

def contains(i, content, substr):
    n = 0
    while(i + n < len(content) and n < len(substr) and content[n + i] == substr[n]):
        n+=1

    return n == len(substr)

def literal(i, content, fin):


    lit = ""
    while(i < len(content) and content[i].isalpha()):
        lit += content[i]
        i += 1

    return (lit, i -1)

def tokenize(content):
    tokens = list()
    i = 0

    while(i < len(content)):

        if(content[i] == '<'):
            (ltrl, i) = literal(i + 1, content, '>')
            tokens.append((Token.NTS, ltrl))
        
        if(content[i] == '('): 
            tokens.append((Token.LNBRACKET, ""))

        if(content[i] == ')'): 
            tokens.append((Token.RNBRACKET, ""))

        if(content[i] == '['): 
            tokens.append((Token.LEBRACKET, ""))
        if(content[i] == ']'): 
            tokens.append((Token.REBRACKET, ""))
        
        if(content[i] == '{'): 
            tokens.append((Token.LBRACKET, ""))
        if(content[i] == '}'): 
            tokens.append((Token.RBRACKET, ""))

        if(content[i] == '|'): 
            tokens.append((Token.OR, ""))
        
        if(content[i] == ';'):
            tokens.append((Token.SEMICOLON, ""))

        if(content[i] == '\''):
            (ltrl, i) = literal(i + 1, content, '\'')
            tokens.append((Token.TS, ltrl))

        if(contains(i, content, "::=")):
            i += 3
            tokens.append((Token.ASSIGN, ""))

        #if(contains(i, content, "(*")):
        #    (ltrl, i) = literal(i + 2, content, '*)')
        #    tokens.append((Token.COMENT, ltrl))

        if(content[i-1] == " " and content[i].isalpha()):
            (ltrl, i) = literal(i, content, ' ')
            tokens.append((Token.TS, ltrl.upper()))

        i += 1
    return tokens







class DataType:
    def __init__(self, tokens, t):

        terminals = []
        for token in tokens:
            if(token[0] == t and not token in terminals):
                terminals.append(token)

        self.terminals = terminals

    def toString(self, type):
        content = f"datatype {type}\n    = "
        
        for term in self.terminals:
            content += f"{term[1]}\n    | "

        content = content[:-7]

        content += f"\n\nval string_of_{type} =\n    fn "

        for term in self.terminals:
            content += f"{term[1]} => \"{term[1]}\"\n    | "
        content = content[:-7]

        
        return content + "\n"


class Parser:
    def __init__(self, tokens) -> None:
        (val, i) = self.commands(tokens, 0)
        self.sml = "[\n"+ val


    def commands(self, tokens, i):

        if (i >= len(tokens)):
            return ("]", i)
        print(f"{i}\tcommands\t{tokens[i]}")

        (v1, i) = self.command(tokens, i)
        (v2, i) = self.commands(tokens, i)

        split = "\n"
        if(v2 != "]"):
            split = ",\n"

        
        return (f"{v1 + split +  v2}", i)

    def command(self, tokens, i):
        print(f"{i}\tcommand\t{tokens[i]}")
        

        (nts, i) = self.nts(tokens,i)
        (assing, i) = self.token(tokens, Token.ASSIGN, i,True)
        (opts, i) = self.options(tokens, i)


        (val, i) = self.token(tokens, Token.SEMICOLON, i, True)

        return (f"    ({ nts }, {opts})", i)


    def token(self, tokens, tkn, i, ex):   
        print(f"{i}\ttoken\t{tokens[i]}")

        if(tokens[i][0] == tkn):
            return (f"{tokens[i][1]}", i + 1)
        if(ex):
            raise Exception(f"syntax error {tokens[i]} {i} {tkn}")
        return("", i+1)
    
    def nts(self, tokens, i):
        print(f"{i}\tnts\t{tokens[i]}")

        if(tokens[i][0] == Token.NTS):
            return (f"{tokens[i][1]}", i + 1)

        raise Exception(f"nts\t{tokens[i]} {i} ")

    def ts(self, tokens, i):
        print(f"{i}\tts\t{tokens[i]}")

        if(tokens[i][0] == Token.TS):
            return (f"{tokens[i][1]}", i + 1)

        raise Exception(f"ts {tokens[i]} {i} ")

    def symbol(self, tokens, i):
        print(f"{i}\tsymbol\t{tokens[i]}")

        try:
            (nts, i) = self.nts(tokens, i)
            return ("N " + nts, i)
        except Exception as ntse:    
            (ts, i) = self.ts(tokens, i)
            return ("T " +ts, i)
        

    def options(self, tokens, i):
        print(f"{i}\toptions\t{tokens[i]}")

        print(f"{i}\toptions ")
        (opt, i) = self.option(tokens, i)
        print(f"{i}\toptions ")

        (optItem, i) = self.optionsItem(tokens, i)
        return (f"[[{opt}{optItem}", i)

    def optionsItem(self, tokens, i):
        print(f"{i}\toptionsItem\t{tokens[i]}")

        if(tokens[i][0] == Token.OR):
            (token, i) = self.symbol(tokens, i + 1)
            (opt, i) = self.optionsItem(tokens, i)
            return (f"\n             ,[{token + opt}", i)
    
        return ("]", i)


    def option(self, tokens, i):
        print(f"{i}\toption\t{tokens[i]}")


        if(tokens[i][0] == Token.NTS or tokens[i][0] == Token.TS):
            print(f"{i}\toption1\t{tokens[i]}")

            (item, i) = self.symbol(tokens, i)
            print(f"{i}\toption2\t{tokens[i]}")

            (items, i) = self.option(tokens, i)

            print(f"{i}\toption3\t{tokens[i]}")

            split = ""
            if(items != "]"):
                split = ", "

            return (f"{item + split + items}" , i)
        
        return ("]", i)

def main():
    content = open("grammar.ebnf", "r").read().replace("\n", " ")
    tokens = tokenize(content)
    nts = DataType(tokens, Token.NTS).toString("term")
    ts = DataType(tokens, Token.TS).toString("nonterm")


    expr = Parser(tokens).sml


    file = f"""
(*
generated by jku, ica
*)
{nts}
{ts}
val string_of_gramsym = (string_of_term, string_of_nonterm)
local
  open FixFoxi.FixFoxiCore
in
val productions = {expr}

val S = expr
val result = fix_foxi productions S string_of_gramsym
end (* local *)
"""
    open("Grammar_generated.sml", "w").write(file)




main()