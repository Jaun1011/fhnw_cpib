
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
    EPSILON = "EPSILON"

def contains(i, content, substr):
    n = 0
    while(i + n < len(content) and n < len(substr) and content[n + i] == substr[n]):
        n+=1

    return n == len(substr)

def literal(i, content):
    lit = ""
    if(content[i].isalpha()):
        lit += content[i]
        i += 1

    while(i < len(content) and (content[i].isalpha() or content[i].isnumeric())):
        lit += content[i]
        i += 1

    return (lit, i - 1)


def tokenize(content):
    tokens = list()
    i = 0

    while(i < len(content)):
        if(content[i:i + 2] == "(*"):
            while(content[i:i + 2] != "*)"):
                i+=1
            i+=2
            
            # tokens.append((Token.COMENT, ltrl))

        if(content[i] == '$'):
            tokens.append((Token.EPSILON, ""))

        if(content[i] == '<'):
            (ltrl, i) = literal(i + 1, content)
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
            (ltrl, i) = literal(i + 1, content)
            tokens.append((Token.TS, ltrl))

        if(contains(i, content, "::=")):
            i += 3
            tokens.append((Token.ASSIGN, ""))

        if((content[i-1] == " " or content[i-1] == "[" or content[i-1] == "{" or content[i-1] == "("or content[i-1] == "|") and content[i].isalpha()):
            (ltrl, i) = literal(i, content)
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
        self.optCount = 0
        self.repCount = 0
        self.joiceCount = 0

        (val, _) = self.commands(tokens, 0)
        self.sml = "[\n" + val +  "]"
        

    def commands(self, tokens, i):

        if (i >= len(tokens)):
            return ("", i)

        print(f"{i}\tcommands\t{tokens[i]}")


        (v1, i) = self.command(tokens, i)
        (v2, i) = self.commands(tokens, i)


        split = "\n"
        if(v2 != ""):
            split = ",\n"

        
        return (f"{v1 + split +  v2}", i)

    def command(self, tokens, i):
        print(f"{i}\tcommand\t{tokens[i]}")
        
        (nts, i) = self.nts(tokens,i)
        (_, i) = self.token(tokens, Token.ASSIGN, i,True)
        (opts, i) = self.options(tokens, i)
        (_, i) = self.token(tokens, Token.SEMICOLON, i, True)

        return (f"    ({ nts },\n        [{opts})", i)

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

    def epsilon(self, tokens,i):
        print(f"{i}\tepsilon\t{tokens[i]}")
        if(tokens[i][0] == Token.EPSILON):
            return ("", i + 1)
        raise Exception(f"epsilon {tokens[i]} {i} ")

    def isSymbol(self, tokens, i):
        return tokens[i][0] == Token.NTS or tokens[i][0] == Token.TS or tokens[i][0] == Token.EPSILON

    def symbol(self, tokens, i):
        print(f"{i}\tsymbol\t{tokens[i]}")

        try:
            (nts, i) = self.nts(tokens, i)
            return ("N " + nts, i)
        
        except Exception as ntse:    
            try:
                (ts, i) = self.ts(tokens, i)
                return ("T " +ts, i)
            except Exception as tse:    
                (ts, i) = self.epsilon(tokens, i)
                return (ts, i)

    def options(self, tokens, i):
        print(f"{i}\toptions\t{tokens[i]}")

        (opt, i) = self.option(tokens, i)
        (optItem, i) = self.optionsItem(tokens, i)


        return (f"[{opt}]{optItem}]", i)

    def optionsItem(self, tokens, i):
        print(f"{i}\toptionsItem\t{tokens[i]}")

        if(tokens[i][0] == Token.OR):
            (token, i) = self.option(tokens, i + 1)
            (opt, i) = self.optionsItem(tokens, i)
            return (f"\n        ,[{token}]{opt}", i)
    
        return ("", i)

    def optionOptFollow(self, tokens, i):
        name = f"opt{self.optCount}"

        self.optCount += 1

        tokens.append((Token.NTS, name))
        tokens.append((Token.ASSIGN, ""))

        while(tokens[i][0] != Token.REBRACKET):
            print(f"{i}\toptionOptFollow\t{tokens[i]}")

            tokens.append(tokens[i])
            i+=1

        tokens.append((Token.OR, ""))
        tokens.append((Token.EPSILON, ""))
        tokens.append((Token.SEMICOLON, ""))

        return (f"N {name}", i+1) 

    def optionRepFollow(self, tokens, i):
        name = f"rep{self.repCount}"

        self.repCount += 1

        tokens.append((Token.NTS, name))
        tokens.append((Token.ASSIGN, ""))

        while(tokens[i][0] != Token.RBRACKET):
            print(f"{i}\toptionRepFollow\t{tokens[i]}")
            tokens.append(tokens[i])
            i+=1

        tokens.append((Token.NTS, name))
        tokens.append((Token.OR, ""))
        tokens.append((Token.EPSILON, ""))
        tokens.append((Token.SEMICOLON, ""))

        return (f"N {name}", i+1) 

    def optionJoiceFollow(self, tokens, i):
        name = f"joice{self.joiceCount}"

        self.joiceCount += 1

        tokens.append((Token.NTS, name))
        tokens.append((Token.ASSIGN, ""))

        while(tokens[i][0] != Token.RNBRACKET):
            print(f"{i}\toptionJoiceFollow\t{tokens[i]}")
            tokens.append(tokens[i])
            i+=1

        tokens.append((Token.SEMICOLON, ""))

        return (f"N {name}", i+1) 


    def option(self, tokens, i):
        print(f"{i}\toption\t{tokens[i]}")
        
        if(tokens[i][0] == Token.LEBRACKET):

            (val, i) = self.optionOptFollow(tokens, i+1)
            (val2, i) = self.option(tokens, i)
            
            if(val2 != ""):
                val2 = ", " + val2

            return (val + val2, i)
            
        if(tokens[i][0] == Token.LBRACKET):

            (val, i) = self.optionRepFollow(tokens, i+1)
            (val2, i) = self.option(tokens, i)

            if(val2 != ""):
                val2 = ", " + val2

            return (val + val2, i)

        if(tokens[i][0] == Token.LNBRACKET):
            (val, i) = self.optionJoiceFollow(tokens, i+1)
            (val2, i) = self.option(tokens, i)

            if(val2 != ""):
                val2 = ", " + val2
            return (val + val2, i)            

        if(self.isSymbol(tokens, i)):
            (item, i) = self.symbol(tokens, i)
            (items, i) = self.option(tokens, i)

            split = ""
            if(items != ""):
                split = ", "

            return (f"{item + split + items}" , i)
        return ("", i)

def main():
    content = open("grammar.ebnf", "r").read().replace("\n", " ")
    tokens = tokenize(content)
    print(tokens)

    parser = Parser(tokens)

    nts = DataType(tokens, Token.NTS).toString("nonterm")
    ts = DataType(tokens, Token.TS).toString("term")

    file = f"""
(*
generated by jku, ica
time = {datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")}
*)
{nts}
{ts}
val string_of_gramsym = (string_of_term, string_of_nonterm)
local
  open FixFoxi.FixFoxiCore
in
val productions = {parser.sml}

val S = program
val result = fix_foxi productions S string_of_gramsym
end (* local *)
"""
    open("Grammar_generated.sml", "w").write(file)



main()