#include "value.h"
#include "linkedlist.h"
#include "talloc.h"
#include "tokenizer.h"
#include "parser.h"
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include "interpreter.h"



void evaluationError(char *errorMessage)
{
  printf("Evaluation error: %s\n", errorMessage);
  texit(1);
}

// tree should just be a single cell
void print(Value* tree)
{
  switch (tree->type) //segfault for let04
  {
    case INT_TYPE :
      printf("%d", tree->i);
      break;
    case DOUBLE_TYPE :
      printf("%lf",tree->d);
      break;
    case STR_TYPE :
      printf("%s",tree->s);
      break;
    case BOOL_TYPE :
      if (tree->i == 1)
      {
        printf("#t");
      } else
      {
        printf("#f");
      }
      break;
    case CLOSURE_TYPE :
      printf("#<procedure>");
      break;
    case VOID_TYPE :
      break;
    case CONS_TYPE :
      printTree(tree);
      break;
    default :
      evaluationError("Print error");
  }
}


// Returns the length of a given tree. Tree should be a list of CONS_TYPEs followed by a NULL_TYPE
int treeLength(Value *tree)
{
  int len = 0;
  Value *cur = tree;
  while (cur->type != NULL_TYPE)
  {
    if (cur->type != CONS_TYPE)
    {
      printf("Error in treeLength(). CONS_TYPE node expected\n");
      texit(1);
    }
    len++;
    cur = cdr(cur);
  }
  return len;
}

// This method looks up the given symbol within the bindings to see if it is a bound variable
Value *lookUpSymbol(Value *symbol, Frame *frame)
{
  Value *bindings = frame->bindings; // we have to choose how to implement this. my idea is a list of cons cells, where each cell points to a *pair* of
                                     // cons cells, which are (symbol, value)
                                     // so (let ((x 1) (y "a")) ...) would look like *bindings = CONS-------------->CONS
                                     //                                                            |                   |
                                     //                                                           CONS--->CONS       CONS--->CONS
                                     //                                                            |        |         |        |
                                     //                                                        SYMBOL(x)   INT(1)  SYMBOL(y)  STR("a")
  Value *cur = bindings;
  while (cur->type != NULL_TYPE)
  //while (cur->type == CONS_TYPE)
  {
    //assert(cur->type == CONS_TYPE && "Should be cons type");
    Value *pairList = car(cur);
    //assert(pairList != NULL && pairList->type == CONS_TYPE);
    Value *boundSymbol = car(pairList);
    assert(boundSymbol->type == SYMBOL_TYPE);
    if (!strcmp(boundSymbol->s, symbol->s)) // if boundSymbol is equal to symbol, return the boundValue
    {
      Value *boundValue = car(cdr(pairList));
      if (boundValue->type == SYMBOL_TYPE)
      {
        return lookUpSymbol(boundValue, frame->parent);
      } else if (boundValue->type == CONS_TYPE)
      {
        return eval(cdr(pairList), frame);
      }
      return boundValue;
    }
    cur = cdr(cur);
  }
  if (frame->parent == NULL)
  {
    return NULL;
  }
  return lookUpSymbol(symbol, frame->parent);
}


Value *evalQuote(Value *tree){
  if (treeLength(tree) != 1){
    evaluationError("Error: too many args in quote");
  }
  if (tree->type == NULL_TYPE){
    evaluationError("Error: Quote args");
  }
  return tree;
}

Value *evalIf(Value *args, Frame *frame)
{
  if (treeLength(args) != 3)
  {
    evaluationError("evalution error");
  }
  Value* boolVal = eval(args, frame);
  if (boolVal->type != BOOL_TYPE)
  {
    evaluationError("Error: 1st argument of IF is not BOOL_TYPE");
  }
  else
  {
    if (boolVal->i == 1)
    {
      return eval(cdr(args), frame);
    }
    else
    {
      return eval(cdr(cdr(args)), frame);
    }
  }
  return NULL;
}

Value *evalLet(Value *args, Frame *frame)
{
  Frame *newFrame = talloc(sizeof(Frame));
  newFrame->bindings = frame->bindings;
  newFrame->parent = frame;
  if (treeLength(args) < 1){
    evaluationError("Error: empty arguments to let");
  } else {
    newFrame->bindings = car(args);
    //appendBindingsTree(newFrame->bindings, frame->bindings);
    
    Value* next = cdr(args);
    return eval(next, newFrame);
  }
  return NULL;
}

Value *evalEach(Value *args, Frame *frame){
  Value *evaledArgs = makeNull();
  Value *cur = args;
  while (cur->type != NULL_TYPE){
    evaledArgs = cons(eval(cur, frame), evaledArgs);
    cur = cdr(cur);
  }
  return reverse(evaledArgs);
}

Value *apply(Value *function, Value *args){
  if (args->type == NULL_TYPE)
  {
    eval(function->cl.functionCode, function->cl.frame);
  }
  //Construct a new frame whose parent frame is the environment 
  //stored in the closure.
  Frame *newFrame = talloc(sizeof(Frame));
  newFrame->parent = function->cl.frame;
  //Add bindings to the new frame mapping each formal parameter 
    //(found in the closure) to the corresponding actual parameter (found in args).
  Value *bindings = makeNull();
  Value *pnames = function->cl.paramNames;
  Value *body = function->cl.functionCode;
  while (args->type != NULL_TYPE && pnames->type != NULL_TYPE) {
    Value *binding = cons(car(args), makeNull());
    binding = cons(car(pnames), binding);
    bindings = cons(binding, bindings);
    pnames = cdr(pnames);
    args = cdr(args);
  }
  newFrame->bindings = bindings;
  //Evaluate the function body (found in the closure) with the new 
  //frame as its environment, and return the result of the call to eval.
  return eval(body, newFrame);
}

Value *evalDefine(Value *args, Frame *frame){
  if (args->type == NULL_TYPE) {
    evaluationError("Evaluation error: no arguments");
  }
  else if (cdr(args)->type == NULL_TYPE || car(cdr(args))->type == NULL_TYPE) {
    evaluationError("Evaluation error: no body");
  } else if (car(args)->type != SYMBOL_TYPE) {
    evaluationError("Evaluation error: not a symbol");
  }
  //segfault
  Value *binding = cons(eval(cdr(args), frame), makeNull());
  binding = cons(car(args), binding);
  binding = cons(binding, makeNull());
  if (frame->bindings->type == NULL_TYPE)
  {
    frame->bindings = binding;
  } else
  {
    Value* tail = binding;
    tail->c.cdr = frame->bindings;
    frame->bindings = binding;
  }
  
  Value *special = talloc(sizeof(Value));
  special->type = VOID_TYPE;
  return special;
}

Value *evalLambda(Value *args, Frame *frame){
  if (args->type == NULL_TYPE) {
    evaluationError("Evaluation Error: empty lambda");
  } else if (length(args) != 2) {
    evaluationError("Evaluation Error: parameters or body missing");
  } else if (car(args)->type == CONS_TYPE && car(car(args))->type != SYMBOL_TYPE) {
    evaluationError("Evaluation Error: parameters must be symbols");
  }
  Value* closure = (Value*)talloc(sizeof(Value));
  closure->type = CLOSURE_TYPE;
  closure->cl.frame = frame;
  if (car(args)->type == NULL_TYPE)
  {
    closure->cl.paramNames = makeNull();
  } else
  {
    assert(car(args)->type == CONS_TYPE && "Error, lambda parameter tree is weirdly formatted\n");
    closure->cl.paramNames = car(args);
  }
  closure->cl.functionCode = cdr(args);

  return closure;
}



Value *eval(Value *tree, Frame *frame)
{
  Value* val = car(tree);
  switch (val->type)
  {
    case INT_TYPE: // this means the whole program consists of one single number, so we can just return the number.
      return val;

    case DOUBLE_TYPE:
      return val;
    case BOOL_TYPE:
      return val;
    case NULL_TYPE:
      return val;
    case STR_TYPE: // this means the whole program is just a string, so we can just return it
      return val;
    case SYMBOL_TYPE:
    {               // this means that the whole program is just a variable name, so just return the value of the variable.
      Value* found = lookUpSymbol(val, frame);
      if (found == NULL)
      {
        evaluationError("symbol not found.\n");
      }
      return found;
    }
    case CONS_TYPE:
      if (!strcmp(car(val)->s, "if"))
      {
        return evalIf(cdr(val), frame);
      }
      if (!strcmp(car(val)->s, "quote"))
      {
        return evalQuote(cdr(val));
      }
      if (!strcmp(car(val)->s, "let"))
      {
        return evalLet(cdr(val), frame);
      }
      // .. other special forms here...
      if (!strcmp(car(val)->s, "define")) 
      {
        //Frame *defineFrame = talloc(sizeof(Frame));
        return evalDefine(cdr(val), frame);
      }
      if (!strcmp(car(val)->s, "lambda")) 
      {
        return evalLambda(cdr(val), frame);
      }
      else
      {
        // If not a special form, evaluate the first, evaluate the args, then
        // apply the first to the args.
        Value *evaledOperator = eval(val, frame); // this returns a closure
        Value *evaledArgs = evalEach(cdr(val), frame);
        return apply(evaledOperator, evaledArgs);
      }
      break;
    default:
      printf("Evaluation error, type: %u,\n",val->type);
      break;
  }
  return NULL;
}


// calls eval for each top-level S-expression in the program.
// You should print out any necessary results before moving on to the next S-expression.
void interpret(Value *tree)
{
  Frame *frame = talloc(sizeof(Frame));
  frame->parent = NULL;
  frame->bindings = makeNull();
  // for s-expression in program:
  Value *curr = tree;
  while (curr->type != NULL_TYPE)
  {
    Value* result = eval(curr,frame);
    print(result);
    curr = cdr(curr);
    printf("\n");
  }
}



// int main() {

//     Value *list = tokenize();
//     Value *tree = parse(list);
//     interpret(tree);

//     tfree();
//     return 0;
// }