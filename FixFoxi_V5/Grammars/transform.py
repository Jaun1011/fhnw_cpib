
from os import remove
from typing import Literal
import re


def nts(content):
    lines = content.replace("';'", "'SEMICOLON'").split(";")

    nonterminals = []
    for line in lines:
        nt = line.split("::=")[0]\
            .replace("<", "")\
            .replace(">", "")\
            .replace("\n", "")\
            .replace(" ","")


        if(nt != ""):
            nonterminals.append(nt)
    
    return nonterminals

def ts(content):

    lines = content\
        .replace("';'", "'SEMICOLON'")\
        .replace("'\n'", "'NEWLINE'")\
        .split(";")

    ts = []
    for line in lines:
        
        assings = line.split("::=")
        if(len(assings) >= 2):
            assign = line.split("::=")[1]
            removed = re.sub("\<.*\>|\'.*\'", " ", assign)

            ats = removed\
                .replace("\n","")\
                .replace("[","")\
                .replace("]","")\
                .replace(" ","|")\
                .split("|")

            for sub in ats:
                sub = sub\
                    .replace(" ", "")\
                    .replace("*", "")\
                    .replace("{", "")\
                    .replace("}", "")
                    
                if(not (sub == "" or  "(" in sub or ")" in sub or "-" in sub)):
                    ts.append(sub)
    return ts

def transform(identifier, types):
    content = "datatype "  + identifier + "\n    ="

    for type in types:
        content += " " + type.upper() + "\n    |"

    content = content[:-5]

    content += f"val string_of_{identifier} = \n    fn"
    for type in types:
        type = type.upper()
        content += f" {type} => \"{type}\"\n    |"
    
    content = content[:-1]

    return content


def hack():
    content = open("grammar.ebnf", "r").read()
    content = re.sub("\(\*.*\*\)", "", content)


    tsym = nts(content)
    print(transform("nonterm", tsym))







def main():
    hack()
    



main()