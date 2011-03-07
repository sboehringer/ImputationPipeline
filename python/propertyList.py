#!/bin/python

import re
import sys

def quoteBackslashAndQuotes (text):
    return re.sub("([\"]|[\\\\])",r'\\\1',text)
    
def wrapString (text):
    return text if re.match("^([_\/\-a-zA-Z0-9.]+)$",text) else '"'+text+'"'
    
def propertyFromStringFast (plist):
    stringRE='(?:(?:[_\/\-a-zA-Z0-9.]+)|(?:\"(?:(?:\\\\.)*(?:[^"\\\\]+(?:\\\\.)*)*)\"))'
    commentRE='(?:/\*(?:.*?)\*/)';	# formerly '\s*'
    plist=re.sub(commentRE,'',plist)
    #tokens=re.split('$'+stringRE+'{,}|[(]|[)]|[{]|[}]|[=]|[,]|[;]|<.*?>',plist)
    tokens=re.findall(stringRE+'+|[(]|[)]|[,]|[{]|[}]|[=]|[;]|<.*?>',plist)
    return propertyFromStringFastRaw (tokens)

def propertyFromStringFastRaw (tokens):
    try:
        token=tokens.pop(0)
    except IndexError:
        sys.exit("out of tokens")
    if token=='(': # we have an array here 	# ')' (bracket)
        array=[]
        while token!=')': 
            array.append(propertyFromStringFastRaw(tokens))
            try:
                token = tokens.pop(0)	# comma or parenthesis
            except IndexError:
                sys.exit("array termination")
            if (token != ',' and token != ')'):	# '('
                sys.exit("array")
        return array
    elif token=='{':
        mydict={}
        token=tokens.pop(0)
        while token!='}':	# '{' (bracket)
            if tokens.pop(0)!='=':
                sys.exit("dict key")
            mydict[token]=propertyFromStringFastRaw(tokens)
            if tokens.pop(0)!=';':
                sys.exit("dict value")
            try:
                token=tokens.pop(0)
            except IndexError:
                sys.exit("dict termination")
        return mydict
    else: # string
        mymatch=re.match('^\"(.*)\"$',token)
        if mymatch:
            return mymatch.group(1)
        else:
            return token

def stringFromProperty (specs,ident):
    myStr=""
    if type(specs)==str:
        myStr=myStr+'"'+quoteBackslashAndQuotes(specs)+'"'
    elif type(specs)==list:
        array="("
        for i in range(len(specs)):
            array=array+'\n'+(ident+1)*'\t'
            array=array+stringFromProperty(specs[i],ident+1)
            if i<len(specs)-1:
                array=array+","
        array=array+'\n'+ident*'\t'
        array=array+")"
        myStr=myStr+array
    elif type(specs)==dict:
        array="{"
        for key in specs.keys():
            array=array+'\n'+(ident+1)*'\t'
            array=array+wrapString(key)+" = " + stringFromProperty(specs[key],ident+1) + ";"
        array=array+'\n'+ident*'\t'
        array=array+"}"
        myStr=myStr+array
    return myStr    
