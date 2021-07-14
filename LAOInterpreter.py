"""
--------------------------------------------
LAO Language Interpreter 
Created by Justin De La Cruz

"""

from sly import Lexer
from sly import Parser

class CalcLexer(Lexer):
    # Set of token names.


    tokens = {IF_STATEMENT, EXIT_STATEMENT, READ_STATEMENT, THEN_STATEMENT, PRINT_STATEMENT,
              INT_VARIABLE, REAL_VARIABLE, STRING_VARIABLE, NUMBER, DECIMAL, STRING,
              PLUS_ARITHMETIC_OPERATOR, MINUS_ARITHMETIC_OPERATOR, TIMES_ARITHMETIC_OPERATOR,
              DIVIDE_ARITHMETIC_OPERATOR, ASSIGN, GT_RELATIONAL_OPERATOR, EQ_RELATIONAL_OPERATOR, LT_RELATIONAL_OPERATOR,
              LE_RELATIONAL_OPERATOR,
              GE_RELATIONAL_OPERATOR, NE_RELATIONAL_OPERATOR, NOT_LOGICAL_OPERATOR, OR_LOGICAL_OPERATOR,
              AND_LOGICAL_OPERATOR}

    # String containing ignored characters between tokens

    ignore = ' \t'

    # Regular expression rules for tokens

    IF_STATEMENT = r'IF|if|If'
    THEN_STATEMENT = r'THEN|then|Then'
    READ_STATEMENT = r'READ|read|Read'
    PRINT_STATEMENT = r'PRINT|print|Print'
    EXIT_STATEMENT = r'END.|end.|End.'
    COMMENT_STATEMENT = r'REM.*|rem.*|Rem.*'


    INT_VARIABLE = r'[a-f|A-F][a-z|A-Z_]*'
    REAL_VARIABLE = r'[g-n|G-N][a-z|A-Z_]*'
    STRING_VARIABLE = r'[o-z|O-Z][a-z|A-Z_]*'
    STRING = r'\".*?\"'

    # Arithmetic Operators
    PLUS_ARITHMETIC_OPERATOR = r'.ADD.|.add.'
    MINUS_ARITHMETIC_OPERATOR = r'.SUB.|.sub.'
    TIMES_ARITHMETIC_OPERATOR = r'.MUL.|.mul.'
    DIVIDE_ARITHMETIC_OPERATOR = r'.DIV.|.div.'

    ASSIGN = r'='

    # Relational Operators
    GT_RELATIONAL_OPERATOR = r'.GT.|.gt.'
    LT_RELATIONAL_OPERATOR = r'.LT.|.lt.'
    EQ_RELATIONAL_OPERATOR = r'.EQ.|.eq.'
    GE_RELATIONAL_OPERATOR = r'.GE.|.ge.'
    LE_RELATIONAL_OPERATOR = r'.LE.|.le.'
    NE_RELATIONAL_OPERATOR = r'.NE.|.ne.'

    # Logical Operators
    NOT_LOGICAL_OPERATOR = r'.NOT.|.not.'
    OR_LOGICAL_OPERATOR = r'.OR.|.or.'
    AND_LOGICAL_OPERATOR = r'.AND.|.and.'

    ignore_newline = r'\n+'

    def __init__(self):
        self.nesting_level = 0

    @_(r'[-+]?[0-9]+(\.([0-9]+)?([eE][-+]?[0-9]+)?|[eE][-+]?[0-9]+)')
    def DECIMAL(self, t):
        t.value = float(t.value)
        return t

    @_(r'[-+]?[1-9][0-9]*')
    def NUMBER(self,t):
        t.value = int(t.value)
        return t

    @_(r'\".*?\"')
    def STRING(self, t):
        t.value = str(t.value[1:-1])
        return t

    @_(r'\REM.*')
    def COMMENT_STATEMENT(self,t):
        print('*comment_statement*', t.value)
        pass

    @_(r'READ|read|Read')
    def READ_STATEMENT(self, t):
        return t

    @_(r'END.|end.|End.')
    def EXIT_STATEMENT(self, t):
        exit()

    # Line number tracking
    @_(r'\n+')
    def ignore_newline(self, t):
        self.lineno += t.value.count('\n')

    def error(self, t):
        print("Illegal character '%s'" % t.value[0])
        self.index += 1

class CalcParser(Parser):
    tokens = CalcLexer.tokens

    precedence = (
        ('left', PLUS_ARITHMETIC_OPERATOR, MINUS_ARITHMETIC_OPERATOR),
        ('left', TIMES_ARITHMETIC_OPERATOR, DIVIDE_ARITHMETIC_OPERATOR),
        ('right', UMINUS)
    )

    def __init__(self):
        self.env = {}

    def error(self, p):                                                # Syntax error
        if p:
            print("Syntax error at token", p.type)
            # Just discard the token and tell the parser it's okay.
            #self.errok()
        else:
            print("Syntax error at EOF")

    @_('')                                                              # Space
    def statement(self, p):
        pass

    @_('PRINT_STATEMENT')
    def statement(self, p):                                             # Print expression
        return ('print_statement')


    # Relational Operators -------------------------------------------

    @_('expr GT_RELATIONAL_OPERATOR expr')                              # GT_RELATIONAL_OPERATOR: .GT.|.gt.
    def condition(self, p):
        #print ('<relational_operator>', p.expr0, p.expr1)
        return('gt_relational_operator', p.expr0, p.expr1)

    @_('expr LT_RELATIONAL_OPERATOR expr')                              # LT_RELATIONAL_OPERATOR: .LT.|.lt.
    def condition(self, p):
        #print ('<relational_operator>', p.expr0, p.expr1)
        return ('lt_relational_operator', p.expr0, p.expr1)

    @_('expr EQ_RELATIONAL_OPERATOR expr')                              # EQ_RELATIONAL_OPERATOR: .EQ.|.eq.
    def condition(self, p):
        #print('<relational_operator>', p.expr0, p.expr1)
        return ('eq_relational_operator', p.expr0, p.expr1)

    @_('expr GE_RELATIONAL_OPERATOR expr')                              # GE_RELATIONAL_OPERATOR: .GE.|.ge.
    def condition(self, p):
        #print('<relational_operator>', p.expr0, p.expr1)
        return ('ge_relational_operator', p.expr0, p.expr1)

    @_('expr LE_RELATIONAL_OPERATOR expr')                              # LE_RELATIONAL_OPERATOR: .LE.|.le.
    def condition(self, p):
        #print('<relational_operator>', p.expr0, p.expr1)
        return ('le_relational_operator', p.expr0, p.expr1)

    @_('expr NE_RELATIONAL_OPERATOR expr')                              # NE_RELATIONAL_OPERATOR: .NE.|.ne.
    def condition (self, p):
        #print('<relational_operator>', p.expr0, p.expr1)
        return ('ne_relational_operator', p.expr0, p.expr1)

    # Conditions with .AND. ------------------------------------------

    @_('expr GT_RELATIONAL_OPERATOR expr AND_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')   # expr > expr and exp == exp
    def condition(self, p):
        #print ('gt_and_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3)
        return ('gt_and_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3)

    @_('expr LT_RELATIONAL_OPERATOR expr AND_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr < expr and exp == exp
    def condition(self, p):
        # print('<relational_operator>', p.expr0, p.expr1)
        return ('lt_and_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3)

    @_('expr EQ_RELATIONAL_OPERATOR expr AND_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr == expr and exp == exp
    def condition(self, p):
        # print('<relational_operator>', p.expr0, p.expr1)
        return ('eq_and_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3)

    @_('expr GE_RELATIONAL_OPERATOR expr AND_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr >= expr and exp == exp
    def condition(self, p):
        # print('<relational_operator>', p.expr0, p.expr1)
        return ('ge_and_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3)

    @_('expr LE_RELATIONAL_OPERATOR expr AND_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr <= expr and exp == exp
    def condition(self, p):
        # print('<relational_operator>', p.expr0, p.expr1)
        return ('le_and_eq_condition', p.expr0, p.expr1, p.expr3, p.expr4)

    # Conditions with .OR. ------------------------------------------------

    @_('expr GT_RELATIONAL_OPERATOR expr OR_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr > expr or exp == exp
    def condition(self, p):
        #print('gt_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3)
        return ('gt_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3)

    @_('expr LT_RELATIONAL_OPERATOR expr OR_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr < expr or exp == exp
    def condition(self, p):
        # print('<relational_operator>', p.expr0, p.expr1)
        return ('lt_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3)

    @_('expr EQ_RELATIONAL_OPERATOR expr OR_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr == expr or exp == exp
    def condition(self, p):
        # print('<relational_operator>', p.expr0, p.expr1)
        return ('eq_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3)

    @_('expr GE_RELATIONAL_OPERATOR expr OR_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr >= expr or exp == exp
    def condition(self, p):
        # print('<relational_operator>', p.expr0, p.expr1)
        return ('ge_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3)

    @_('expr LE_RELATIONAL_OPERATOR expr OR_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr <= expr or exp == exp
    def condition(self, p):
        # print('<relational_operator>', p.expr0, p.expr1)
        return ('le_or_eq_condition', p.expr0, p.expr1, p.expr3, p.expr4)

    @_('expr NE_RELATIONAL_OPERATOR expr OR_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr !> expr or exp == exp
    def condition(self, p):
        # print('<relational_operator>', p.expr0, p.expr1)
        return ('ne_or_eq_condition', p.expr0, p.expr1, p.expr3, p.expr4)

    # Other Conditions (Special cases)

    @_('expr GT_RELATIONAL_OPERATOR expr AND_LOGICAL_OPERATOR expr GT_RELATIONAL_OPERATOR expr OR_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr > expr and expr > expr or expr == expr
    def condition(self, p):
        # print('gt_and_le_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3, p.expr4, p.expr5, p.expr6)
        return ('gt_and_gt_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3, p.expr4, p.expr5)

    @_('expr GT_RELATIONAL_OPERATOR expr AND_LOGICAL_OPERATOR expr LT_RELATIONAL_OPERATOR expr OR_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr > expr and expr < expr or expr == expr
    def condition(self, p):
        # print('gt_and_le_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3, p.expr4, p.expr5, p.expr6)
        return ('gt_and_lt_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3, p.expr4, p.expr5)

    @_('expr GT_RELATIONAL_OPERATOR expr AND_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr OR_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr > expr and expr == expr or expr == expr
    def condition(self, p):
        #print('gt_and_le_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3, p.expr4, p.expr5, p.expr6)
        return ('gt_and_eq_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3, p.expr4, p.expr5)

    @_('expr GT_RELATIONAL_OPERATOR expr AND_LOGICAL_OPERATOR expr GE_RELATIONAL_OPERATOR expr OR_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr > expr and expr >= expr or expr == expr
    def condition(self, p):
        # print('gt_and_le_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3, p.expr4, p.expr5, p.expr6)
        return ('gt_and_ge_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3, p.expr4, p.expr5)

    @_('expr GT_RELATIONAL_OPERATOR expr AND_LOGICAL_OPERATOR expr LE_RELATIONAL_OPERATOR expr OR_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr > expr and expr <= expr or expr == expr
    def condition(self, p):
        # print('gt_and_le_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3, p.expr4, p.expr5, p.expr6)
        return ('gt_and_le_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3, p.expr4, p.expr5)

    @_('expr GT_RELATIONAL_OPERATOR expr AND_LOGICAL_OPERATOR expr NE_RELATIONAL_OPERATOR expr OR_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')  # expr > expr and exp != exp or expr == expr
    def condition(self, p):
        # print('gt_and_le_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3, p.expr4, p.expr5, p.expr6)
        return ('gt_and_ne_or_eq_condition', p.expr0, p.expr1, p.expr2, p.expr3, p.expr4, p.expr5)

    # Logical Operators conditions  -------------------------------------------

    @_('expr OR_LOGICAL_OPERATOR expr')                                 # OR_LOGICAL_OPERATOR: .OR.|.or.
    def condition(self, p):
        #print('<logical_operator>', p.expr0, p.expr1)
        return ('or_logical_operator', p.expr0, p.expr1)

    @_('expr AND_LOGICAL_OPERATOR expr')                                # AND_LOGICAL_OPERATOR: .AND.|.and.
    def condition(self, p):
        #print('<logical_operator>', p.expr0, p.expr1)
        return ('and_logical_operator', p.expr0, p.expr1)

    @_('NOT_LOGICAL_OPERATOR expr GT_RELATIONAL_OPERATOR expr')         # NOT GT_RELATIONAL_OPERATOR: .GT.|.gt.
    def condition(self, p):
        # print ('<relational_operator>', p.expr0, p.expr1)
        return ('not_gt_relational_operator', p.expr0, p.expr1)

    @_('NOT_LOGICAL_OPERATOR expr LT_RELATIONAL_OPERATOR expr')         # NOT LT_RELATIONAL_OPERATOR: .GT.|.gt.
    def condition(self, p):
        # print ('<relational_operator>', p.expr0, p.expr1)
        return ('not_lt_relational_operator', p.expr0, p.expr1)

    @_('NOT_LOGICAL_OPERATOR expr EQ_RELATIONAL_OPERATOR expr')         # NOT EQ_RELATIONAL_OPERATOR: .GT.|.gt.
    def condition(self, p):
        # print ('<relational_operator>', p.expr0, p.expr1)
        return ('not_eq_relational_operator', p.expr0, p.expr1)

    @_('NOT_LOGICAL_OPERATOR expr GE_RELATIONAL_OPERATOR expr')         # NOT GE_RELATIONAL_OPERATOR: .GT.|.gt.
    def condition(self, p):
        # print ('<relational_operator>', p.expr0, p.expr1)
        return ('not_ge_relational_operator', p.expr0, p.expr1)

    @_('NOT_LOGICAL_OPERATOR expr LE_RELATIONAL_OPERATOR expr')         # NOT LE_RELATIONAL_OPERATOR: .GT.|.gt.
    def condition(self, p):
        # print ('<relational_operator>', p.expr0, p.expr1)
        return ('not_le_relational_operator', p.expr0, p.expr1)

    @_('NOT_LOGICAL_OPERATOR expr NE_RELATIONAL_OPERATOR expr')         # NOT NE_RELATIONAL_OPERATOR: .GT.|.gt.
    def condition(self, p):
        # print ('<relational_operator>', p.expr0, p.expr1)
        return ('not_ne_relational_operator', p.expr0, p.expr1)

    # LAO Statements ------------------------------------------------

    @_('PRINT_STATEMENT expr')                                          # Print statement | Print
    def statement(self, p):
        #print(p.expr)
        return ('print_statement',p.expr)

    @_('PRINT_STATEMENT STRING')  # Print statement | Print
    def statement(self, p):
        return ('print_statement', p.STRING)

    @_('READ_STATEMENT expr')                                           # Read statement | Read
    def statement(self, p):
        #p = input("Enter value: ")
        return ("read_statement" ,p.expr)

    @_('IF_STATEMENT condition THEN_STATEMENT statement')               # IF condition THEN statement
    def statement(self, p):
        #print('if_statement', p.condition, 'branch', p.statement)
        return ('if_statement', p.condition, ('branch', p.statement))

    @_('EXIT_STATEMENT')                                                # End statement | END.
    def statement(self, p):
        #print('<end_statement>', p.EXIT_STATEMENT)
        return ('end_statement', p.EXIT_STATEMENT)

    @_('expr ASSIGN expr')                                              # Assign expr | expr = expr
    def condition(self, p):
        return ('conditional_expr', p.expr0, p.expr1)

    @_('expr ASSIGN STRING')                                            # Assign String | expr = str
    def condition(self, p):
        return ('conditional_expr', p.expr, p.STRING)


    # Arithmetic Operators -----------------------------------------
    @_('expr')
    def statement(self, p):                                             # expression
        # print(p.expr)
        return p.expr

    @_('expr PLUS_ARITHMETIC_OPERATOR expr')                            # .ADD. expression
    def expr(self, p):
        return ('plus_arithmetic_operator', p.expr0, p.expr1)

    @_('expr MINUS_ARITHMETIC_OPERATOR expr')                           # .SUB. expression
    def expr(self, p):
        return ('minus_arithmetic_operator', p.expr0, p.expr1)

    @_('expr TIMES_ARITHMETIC_OPERATOR expr')                            # .MUL. expression
    def expr(self, p):
        return ('times_arithmetic_operator', p.expr0, p.expr1)

    @_('expr DIVIDE_ARITHMETIC_OPERATOR expr')                           # .DIV. expression
    def expr(self, p):
        return ('divide_arithmetic_operator', p.expr0, p.expr1)

    @_('"-" expr %prec UMINUS')                                          # negative value
    def expr(self, p):
        return -p.expr

     # Variables ---------------------------------------------------------

    @_('INT_VARIABLE')
    def expr(self, p):
       return ('variable_int', p.INT_VARIABLE)

    @_('REAL_VARIABLE')
    def expr(self, p):
        return ('variable_real', p.REAL_VARIABLE)

    @_('STRING_VARIABLE')
    def expr(self, p):
         return ('variable_str', p.STRING_VARIABLE)

    @_('var_assign')
    def statement(self, p):
        return p.var_assign

    @_('INT_VARIABLE ASSIGN expr')
    def var_assign(self, p):
        return ('assign_statement', p.INT_VARIABLE, p.expr)

    @_('REAL_VARIABLE ASSIGN expr')
    def var_assign(self, p):
        return ('assign_statement', p.REAL_VARIABLE, p.expr)

    @_('STRING_VARIABLE ASSIGN expr')
    def var_assign(self, p):
        return ('assign_statement', p.STRING_VARIABLE, p.expr)

    @_('INT_VARIABLE ASSIGN STRING')
    def var_assign(self, p):
        return ('assign_statement', p.INT_VARIABLE, p.STRING)

    @_('REAL_VARIABLE ASSIGN STRING')
    def var_assign(self, p):
         return ('assign_statement', p.REAL_VARIABLE, p.STRING)

    @_('STRING_VARIABLE ASSIGN STRING')
    def var_assign(self, p):
        return ('assign_statement', p.STRING_VARIABLE, p.STRING)

    # Others ----------------------------------------------------------

    @_('NUMBER')
    def expr(self, p):
        return ('number', p.NUMBER)

    @_('DECIMAL')
    def expr(self, p):
        return ('decimal', p.DECIMAL)

    @_('STRING')
    def expr(self, p):
        return ('string', p.STRING)

class BasicExecute:
        def __init__(self, tree, env):
            self.env = env
            result = self.walkTree(tree)
            if result is not None and isinstance(result, int):
                print(result)
            if isinstance(result, str): #or result[0] == '"':
                result = str('"' + result + '"')
                print(result)
            if isinstance(result, float):
                print(result)

        def walkTree(self, node):
            if isinstance(node, int):
                return node
            if isinstance(node, str):
                return node
            if isinstance(node, float):
                return node
            if node is None:
                return None

            # Other -----------------------------------------------------------

            if node[0] == 'program':
                if node == None:
                    self.walkTree(node[2])
                else:
                    self.walkTree(node[1])
                    self.walkTree(node[2])

            if node[0] == 'number':
                return node[1]

            if node[0] == 'str':
                return node[1]

            if node[0] == 'chr':
                return node[1]

            if node[0] == 'string':
                return node[1]

            if node[0] == 'decimal':
                return node[1]

            # LAO Statements ---------------------------------------------------

            if node[0] == 'print_statement':
                #print(self.walkTree(node[1]))
                return self.walkTree(node[1])
            elif node[0] == 'read_statement':
                i = int(input("Enter value: "))
                self.env[node[1][1]] = i
                return node[1]
            elif node[0] == 'comment_statement':
                pass

            if node[0] == 'if_statement':
                result = self.walkTree(node[1])
                if result:
                    return self.walkTree(node[2][1])

            # Arithmetic Operators -----------------------------------------

            if node[0] == 'plus_arithmetic_operator':
                return self.walkTree(node[1]) + self.walkTree(node[2])
            elif node[0] == 'minus_arithmetic_operator':
                return self.walkTree(node[1]) - self.walkTree(node[2])
            elif node[0] == 'times_arithmetic_operator':
                return self.walkTree(node[1]) * self.walkTree(node[2])
            elif node[0] == 'divide_arithmetic_operator':
                return self.walkTree(node[1]) // self.walkTree(node[2])

            # Relational Operators -------------------------------------------

            if node[0] == 'gt_relational_operator':
                return self.walkTree(node[1]) > self.walkTree(node[2])
            elif node[0] == 'lt_relational_operator':
                return self.walkTree(node[1]) < self.walkTree(node[2])
            elif node[0] == 'eq_relational_operator':
                return self.walkTree(node[1]) == self.walkTree(node[2])
            elif node[0] == 'ge_relational_operator':
                return self.walkTree(node[1]) >= self.walkTree(node[2])
            elif node[0] == 'le_relational_operator':
                return self.walkTree(node[1]) <= self.walkTree(node[2])
            elif node[0] == 'ne_relational_operator':
                return self.walkTree(node[1]) != self.walkTree(node[2])

            # Relational Operators (with and statement) -------------------------------------------

            if node[0] == 'gt_and_eq_condition':                                                                                    # expr .gt. expr .and. expr .eq. expr
                return self.walkTree(node[1]) > self.walkTree(node[2]) and self.walkTree(node[3]) == self.walkTree(node[4])
            elif node[0] == 'lt_and_eq_condition':                                                                                  # expr .lt. expr .and. expr .eq. expr
                return self.walkTree(node[1]) < self.walkTree(node[2]) and self.walkTree(node[3]) == self.walkTree(node[4])
            elif node[0] == 'eq_and_eq_condition':                                                                                  # expr .eq. expr .and. expr .eq. expr
                return self.walkTree(node[1]) == self.walkTree(node[2]) and self.walkTree(node[3]) == self.walkTree(node[4])
            elif node[0] == 'ge_and_eq_condition':                                                                                  # expr .ge. expr .and. expr .eq. expr
                return self.walkTree(node[1]) >= self.walkTree(node[2]) and self.walkTree(node[3]) == self.walkTree(node[4])
            elif node[0] == 'le_and_eq_relational_operator':                                                                        # expr .le. expr .and. expr .eq. expr
                return self.walkTree(node[1]) <= self.walkTree(node[2])and self.walkTree(node[3]) == self.walkTree(node[4])
            elif node[0] == 'ne_and_eq_relational_operator':                                                                        # expr .ne. expr .and. expr .eq. expr
                return self.walkTree(node[1]) != self.walkTree(node[2])and self.walkTree(node[3]) == self.walkTree(node[4])

            # Relational Operators (with and statement) -------------------------------------------

            if node[0] == 'gt_or_eq_condition':                                                                                    # expr .gt. expr .or. expr .eq. expr
                return self.walkTree(node[1]) > self.walkTree(node[2]) or self.walkTree(node[3]) == self.walkTree(node[4])
            elif node[0] == 'lt_or_eq_condition':                                                                                  # expr .lt. expr .or. expr .eq. expr
                return self.walkTree(node[1]) < self.walkTree(node[2]) or self.walkTree(node[3]) == self.walkTree(node[4])
            elif node[0] == 'eq_or_eq_condition':                                                                                  # expr .eq. expr .or. expr .eq. expr
                return self.walkTree(node[1]) == self.walkTree(node[2]) or self.walkTree(node[3]) == self.walkTree(node[4])
            elif node[0] == 'ge_or_eq_condition':                                                                                  # expr .ge. expr .or. expr .eq. expr
                return self.walkTree(node[1]) >= self.walkTree(node[2]) or self.walkTree(node[3]) == self.walkTree(node[4])
            elif node[0] == 'le_relational_operator':                                                                              # expr .le. expr .or. expr .eq. expr
                return self.walkTree(node[1]) <= self.walkTree(node[2]) or self.walkTree(node[3]) == self.walkTree(node[4])
            elif node[0] == 'ne_relational_operator':                                                                              # expr .ne. expr .or. expr .eq. expr
                return self.walkTree(node[1]) != self.walkTree(node[2]) or self.walkTree(node[3]) == self.walkTree(node[4])


            # Relational Operators (with and & or statement) -------------------------------------------

            if node[0] == 'gt_and_gt_or_eq_condition':                                                                                                                                  # expr .gt. expr .and. expr .gt. expr .or. expr .eq. expr
                return self.walkTree(node[1]) > self.walkTree(node[2]) and self.walkTree(node[3]) > self.walkTree(node[4]) or self.walkTree(node[5]) == self.walkTree(node[6])
            elif node[0] == 'gt_and_lt_or_eq_condition':                                                                                                                                # expr .gt. expr .and. expr .lt. expr .or. expr .eq. expr
                return self.walkTree(node[1]) > self.walkTree(node[2]) and self.walkTree(node[3]) < self.walkTree(node[4]) or self.walkTree(node[5]) == self.walkTree(node[6])
            elif node[0] == 'gt_and_eq_or_eq_condition':                                                                                                                                # expr .gt. expr .and. expr .eq. expr .or. expr .eq. expr
                return self.walkTree(node[1]) > self.walkTree(node[2]) and self.walkTree(node[3]) == self.walkTree(node[4]) or self.walkTree(node[5]) == self.walkTree(node[6])
            elif node[0] == 'gt_and_ge_or_eq_condition':                                                                                                                                # expr .gt. expr .and. expr .ge. expr .or. expr .eq. expr
                return self.walkTree(node[1]) > self.walkTree(node[2]) and self.walkTree(node[3]) >= self.walkTree(node[4]) or self.walkTree(node[5]) == self.walkTree(node[6])
            elif node[0] == 'gt_and_le_or_eq_condition':                                                                                                                                # expr .gt. expr .and. expr .le. expr .or. expr .eq. expr
                return self.walkTree(node[1]) > self.walkTree(node[2]) and self.walkTree(node[3]) <= self.walkTree(node[4]) or self.walkTree(node[5]) == self.walkTree(node[6])
            elif node[0] == 'gt_and_ne_or_eq_condition':                                                                                                                                # expr .gt. expr .and. expr .ne. expr .or. expr .eq. expr
                return self.walkTree(node[1]) > self.walkTree(node[2]) and self.walkTree(node[3]) != self.walkTree(node[4]) or self.walkTree(node[5]) == self.walkTree(node[6])

            # Logical Operators -------------------------------------------

            if node[0] == 'or_logical_operator':
                return self.walkTree(node[1]) or self.walkTree(node[2])
            elif node[0] == 'and_logical_operator':
                return self.walkTree(node[1]) and self.walkTree(node[2])
            elif node[0] == 'not_gt_relational_operator':
                return not(self.walkTree(node[1]) > self.walkTree(node[2]))
            elif node[0] == 'not_lt_logical_operator':
                return not(self.walkTree(node[1]) < self.walkTree(node[2]))
            elif node[0] == 'not_eq_logical_operator':
                return not(self.walkTree(node[1]) == self.walkTree(node[2]))
            elif node[0] == 'not_ge_logical_operator':
                return not(self.walkTree(node[1]) >= self.walkTree(node[2]))
            elif node[0] == 'not_le_logical_operator':
                return not(self.walkTree(node[1]) <= self.walkTree(node[2]))
            elif node[0] == 'not_ne_logical_operator':
                return not(self.walkTree(node[1]) != self.walkTree(node[2]))

        # Assign Statement -----------------------------------------------

            if node[0] == 'assign_statement':
                self.env[node[1]] = self.walkTree(node[2])
                return node[1]

        # Variables -------------------------------------------------------

            if node[0] == 'variable':
                try:
                    return self.env[node[1]]
                except LookupError:
                    print("Undefined variable '" + node[1] + "' found!")
                    return 0

            if node[0] == 'variable_int':
                try:
                    return self.env[node[1]]
                except LookupError:
                    print("Undefined variable '" + node[1] + "' found!")
                    return 0

            if node[0] == 'variable_real':
                try:
                    return self.env[node[1]]
                except LookupError:
                    print("Undefined variable '" + node[1] + "' found!")
                    return 0

            if node[0] == 'variable_str':
                try:
                    return self.env[node[1]]
                except LookupError:
                    print("Undefined variable '" + node[1] + "' found!")
                    return 0

if __name__ == '__main__':
    print('{:^40}'.format('===LAO Programming Language==='))
    lexer = CalcLexer()
    parser = CalcParser()
    env = {}
    while True:
        try:
            data = input('LAO> ')
        except EOFError:
            break
        if data:
            tree = parser.parse(lexer.tokenize(data))
            BasicExecute(tree, env)
