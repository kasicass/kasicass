// 
// FSMclass == 1:* ==> FSMstate
//

#pragma once

#include <map>

class FSMstate
{
public:
  FSMstate(int stateID, unsigned int transitions);
  ~FSMstate();

  int getID() { return stateID_; }

  void addTransition(int input, int outputID);
  void deleteTransition(int outputID);

  int getOutput(int input);

private:
  unsigned int numberOfTransitions_;  // max number of states supported
  int *inputs_;                       // input array for transitions
  int *outputStates_;                 // ouput state array
  int stateID_;	                      // the unique ID of this state
};


class FSMclass
{
public:
	FSMclass(int stateID);  // initial state
	~FSMclass();

	int getCurrentState() { return currentState_; }
	void setCurrentState(int stateID) { currentState_ = stateID; }

	FSMstate* getState(int stateID);
	void addState(FSMstate *state);
	void deleteState(int stateID);

	int stateTransition(int input);

private:
	typedef std::map<int, FSMstate*, std::less<int>> StateMap;

	StateMap map_;
	int currentState_;
};
