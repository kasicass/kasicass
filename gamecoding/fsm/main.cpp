//
// idle <---> walk
//   ^       /
//    \     V 
//    attack
//
// idle -- see someone --> walk
// walk -- not see someone --> idle
// walk -- be attacked --> attack
// attack -- not be attacked --> idle


#include "fsm.hpp"
#include <stdio.h>

enum NpcInput {
	INPUT_SEE_SOMEONE = 10,
	INPUT_NOT_SEE_SOMEONE,
	INPUT_BE_ATTACKED,
	INPUT_NOT_BE_ATTACKED,
};

enum NpcState {
	STATE_IDLE = 1,
	STATE_WALK,
	STATE_ATTACK
};

char* state2Str(int state)
{
	if (state == STATE_IDLE) return "idle";
	else if (state == STATE_WALK) return "walk";
	else return "attack";
}

int main(void)
{
	FSMclass myfsm(STATE_IDLE);
	FSMstate *pState;

	pState = new FSMstate(STATE_IDLE, 1);
	pState->addTransition(INPUT_SEE_SOMEONE, STATE_WALK);
	myfsm.addState(pState);

	pState = new FSMstate(STATE_WALK, 2);
	pState->addTransition(INPUT_NOT_SEE_SOMEONE, STATE_IDLE);
	pState->addTransition(INPUT_BE_ATTACKED, STATE_ATTACK);
	myfsm.addState(pState);

	pState = new FSMstate(STATE_ATTACK, 1);
	pState->addTransition(INPUT_NOT_BE_ATTACKED, STATE_IDLE);
	myfsm.addState(pState);

	printf("npc = %s, then see someone\n", state2Str(myfsm.getCurrentState()));
	myfsm.stateTransition(INPUT_SEE_SOMEONE);

	printf("npc = %s, then be attacked\n", state2Str(myfsm.getCurrentState()));
	myfsm.stateTransition(INPUT_BE_ATTACKED);

	printf("npc = %s, then not be attacked\n", state2Str(myfsm.getCurrentState()));
	myfsm.stateTransition(INPUT_NOT_BE_ATTACKED);

	printf("npc = %s\n", state2Str(myfsm.getCurrentState()));

	return 0;
}
