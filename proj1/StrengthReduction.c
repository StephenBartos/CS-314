/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *  Fall 2017                                *
 *  Author: Uli                              *
 *  Student Version                          *
 *********************************************
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"
#include <math.h>

int isPowerOfTwo(int);
int getPowerOfTwo(int);

int main()
{
	Instruction *head;

	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}

        // third 2 instructions per iteration
        Instruction *first, *second;
        first = second = NULL;

        first = head;
        while (first){
            int op, shift, rb, rc;
            op = shift = rb = rc = 0;

            second = first->next;
            if (!second) {
                first = first->next;
                continue;
            }
            if (!isLOADI(first)) {
                first = first->next;
                continue;
            }
            if (second->field2 != first->field2) {
                first = first->next;
                continue;
            }
            shift = getPowerOfTwo(first->field1);
            if (shift == -1) {
                first = first->next;
                continue;
            }
            switch (second->opcode) {
                case MUL:
                    op = LSHIFTI; 
                    break;
                case DIV:
                    op = RSHIFTI; 
                    break;
                default:
                    first = first->next;
                    continue;
            }
            rb = second->field1;
            rc = second->field3;
            
            // Apply strength reduction
            first->opcode = op;
            first->field1 = rb;
            first->field2 = shift;
            first->field3 = rc;

            // fix pointers before freeing
            first->next = second->next;

            // delete unoptimized instruction
            free(second);

            // slide window to right by one instruction
            first = first->next;
        }
        if (!head) {
            exit(EXIT_FAILURE);
        }
        PrintInstructionList(stdout, head);
	return EXIT_SUCCESS;
}

int isPowerOfTwo(int x)
{
    return x && !(x & (x-1));
}

int getPowerOfTwo(int x)
{
    if (!isPowerOfTwo(x))
        return -1;
    return (int)log2(x);
}
