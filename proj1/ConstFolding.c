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


int main()
{
	Instruction *head;

	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}

        Instruction *first, *second, *third;
        first = second = third = NULL;

        // next 3 instructions per iteration
        first = head;
        while (first){
            // get next 2 instructions
            second = first->next;
            if (!second) {
                first = first->next;
                continue;
            }
            third = second->next;
            if (!third) {
                first = first->next;
                continue;
            }
            // check if instructions are valid for constant folding
            if (!isLOADI(first) || !isLOADI(second)) {
                first = first->next;
                continue;
            }
            // loadI c2 => rb
            // loadI c1 => ra
            // OP rb, ra => rc
            // ra <=> first->field2
            // rb <=> second->field2
            int ra, rb, c1, c2, c3;
            ra = first->field2;
            rb = second->field2;
            if (third->field1 == ra && third->field2 == rb) { 
                // OP ra, rb => rc
                c1 = first->field1;
                c2 = second->field1;
            }
            else if(third->field1 == rb && third->field2 == ra) {
                // OP rb, ra => rc
                c1 = second->field1;
                c2 = first->field1;
            }
            else {
                first = first->next;
                continue;
            }
            // constant folding optimization
            switch (third->opcode) {
                case ADD:
                    c3 = c1 + c2;
                    break;
                case SUB: 
                    c3 = c1 - c2;
                    break;
                case MUL:
                    c3 = c1 * c2;
                    break;
                default:
                    first = first->next;
                    continue;
            }
            first->field1 = c3;
            first->field2 = third->field3;
            first->next = third->next;

            free(second);
            free(third);
            // slide window to right by one instruction
            first = first->next;
        }
        if (!head) {
            exit(EXIT_FAILURE);
        }
        PrintInstructionList(stdout, head);
	return EXIT_SUCCESS;
}
