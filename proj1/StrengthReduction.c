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

        // next 2 instructions per iteration
        Instruction *ptr, *prev,*next;
        ptr= prev = next =  NULL;

        ptr = head;
        while (ptr){
            int shift, op;
            prev = ptr->prev;
            next = ptr->next;
            if (prev && prev->opcode == LOADI) {

            }
            // slide window to right by one instruction
            ptr = ptr->next;
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
