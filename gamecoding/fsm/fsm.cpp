#include "fsm.hpp"

//
// FSMstate
//

FSMstate::FSMstate(int stateID, unsigned int transitions)
{
	// don't allow 0 transitions
	if (transitions == 0) numberOfTransitions_ = 1;
	else numberOfTransitions_ = transitions;

	stateID_ = stateID;

	inputs_ = new int[numberOfTransitions_];
	for (int i = 0; i < numberOfTransitions_; ++i)
		inputs_[i] = 0;

	outputStates_ = new int[numberOfTransitions_];
	for (int i = 0; i < numberOfTransitions_; ++i)
		outputStates_[i] = 0;
}

FSMstate::~FSMstate()
{
	delete [] inputs_;
	delete [] outputStates_;
}

void FSMstate::addTransition(int input, int outputID)
{
	// inputs_ and outputStates_ are not sorted
	// so find the first non-zero offset in outputStates_[]
	int i;

	for (i = 0; i < numberOfTransitions_; ++i)
	{
		if (!outputStates_[i])
			break;
	}

	if (i < numberOfTransitions_)
	{
		outputStates_[i] = outputID;
		inputs_[i] = input;
	}
}

void FSMstate::deleteTransition(int outputID)
{
	int i;

	for (i = 0; i < numberOfTransitions_; ++i)
	{
		if (outputStates_[i] == outputID)
			break;
	}

	if (i >= numberOfTransitions_)
		return;

	inputs_[i] = 0;
	outputStates_[i] = 0;

	for (; i < (numberOfTransitions_-1); ++i)
	{
		if (!outputStates_[i])
			break;

		inputs_[i] = inputs_[i+1];
		outputStates_[i] = outputStates_[i+1];
	}

	inputs_[i] = 0;
	outputStates_[i] = 0;
}

int FSMstate::getOutput(int input)
{
	int outputID = stateID_;

	for (int i = 0; i < numberOfTransitions_; ++i)
	{
		if (!outputStates_[i])
			break;

		if (input == inputs_[i])
		{
			outputID = outputStates_[i];
			break;
		}
	}

	return outputID;
}


//
// FSMclass
//

FSMclass::FSMclass(int stateID) : currentState_(stateID)
{
}

FSMclass::~FSMclass()
{
	FSMstate *pState = NULL;
	StateMap::iterator it;

	if (!map_.empty())
	{
		for (it = map_.begin(); it != map_.end(); ++it)
		{
			pState = it->second;
			delete pState;
		}
	}
}

FSMstate *FSMclass::getState(int stateID)
{
	FSMstate *pState = NULL;
	StateMap::iterator it;

	if (!map_.empty())
	{
		it = map_.find(stateID);
		if (it != map_.end())
			pState = it->second;
	}

	return pState;
}

void FSMclass::addState(FSMstate *pNewState)
{
	FSMstate *pState = NULL;
	StateMap::iterator it;

	if (!map_.empty())
	{
		it = map_.find(pNewState->getID());
		if (it != map_.end())
			pState = it->second;
	}

	if (pState != NULL)
		return;

	map_.insert( std::make_pair(pNewState->getID(), pNewState) );
}

void FSMclass::deleteState(int stateID)
{
	FSMstate *pState = NULL;
	StateMap::iterator it;

	if (!map_.empty())
	{
		it = map_.find(stateID);
		if (it != map_.end())
			pState = it->second;
	}

	if (pState != NULL && pState->getID() == stateID)
	{
		map_.erase(it);
		delete pState;
	}
}

int FSMclass::stateTransition(int input)
{
	// the current state of the FSM must be set to have a transition
	if (!currentState_)
		return currentState_;

	FSMstate *pState = getState(currentState_);
	if (pState == NULL)
	{
		// error
		currentState_ = 0;
		return currentState_;
	}

	currentState_ = pState->getOutput(input);
	return currentState_;
}

