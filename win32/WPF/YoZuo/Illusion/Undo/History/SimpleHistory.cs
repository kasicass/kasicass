using System;
using System.Collections.Generic;

namespace Illusion
{
    /// <summary>
    /// IActionHistory represents a recorded list of actions undertaken by user.
    /// This class implements a usual, linear action sequence. You can move back and forth
    /// changing the state of the respective document. When you move forward, you execute
    /// a respective action, when you move backward, you Undo it (UnExecute).
    /// Implemented through a double linked-list of SimpleHistoryNode objects.
    /// ====================================================================
    /// </summary>
    internal class SimpleHistory : IActionHistory
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="SimpleHistory"/> class.
        /// </summary>
        public SimpleHistory()
        {
            Init();
        }

        #region Events

        /// <summary>
        /// Occurs when [collection changed].
        /// </summary>
        public event EventHandler CollectionChanged;
        /// <summary>
        /// Raises the undo buffer changed.
        /// </summary>
        protected void RaiseUndoBufferChanged()
        {
            if (CollectionChanged != null)
            {
                CollectionChanged(this, new EventArgs());
            }
        }

        #endregion

        private SimpleHistoryNode mCurrentState = new SimpleHistoryNode();
        /// <summary>
        /// "Iterator" to navigate through the sequence, "Cursor"
        /// </summary>
        /// <value>The state of the current.</value>
        public SimpleHistoryNode CurrentState
        {
            get
            {
                return mCurrentState;
            }
            set
            {
                if (value != null)
                {
                    mCurrentState = value;
                }
                else
                {
                    throw new ArgumentNullException("CurrentState");
                }
            }
        }

        /// <summary>
        /// Gets or sets the head.
        /// </summary>
        /// <value>The head.</value>
        public SimpleHistoryNode Head { get; set; }

        /// <summary>
        /// Gets or sets the last action.
        /// </summary>
        /// <value>The last action.</value>
        public IAction LastAction { get; set; }

        /// <summary>
        /// Adds a new action to the tail after current state. If 
        /// there exist more actions after this, they're lost (Garbage Collected).
        /// This is the only method of this class that actually modifies the linked-list.
        /// </summary>
        /// <param name="newAction">Action to be added.</param>
        /// <returns>true if action was appended, false if it was merged with the previous one</returns>
        public bool AppendAction(IAction newAction)
        {
            if (CurrentState.PreviousAction == null)
            {
                CurrentState.NextAction = newAction;
                CurrentState.NextNode = new SimpleHistoryNode(newAction, CurrentState);
            }
            else
            {
                if (CurrentState.PreviousAction.TryToMerge(newAction))
                {
                    RaiseUndoBufferChanged();
                    return false;
                }
                else
                {
                    CurrentState.NextAction = newAction;
                    CurrentState.NextNode = new SimpleHistoryNode(newAction, CurrentState);
                }
            }
            return true;
        }

        /// <summary>
        /// All existing Nodes and Actions are garbage collected.
        /// </summary>
        public void Clear()
        {
            Init();
            RaiseUndoBufferChanged();
        }

        /// <summary>
        /// Inits this instance.
        /// </summary>
        private void Init()
        {
            CurrentState = new SimpleHistoryNode();
            Head = CurrentState;
        }

        /// <summary>
        /// Enums the undoable actions.
        /// </summary>
        /// <returns></returns>
        public IEnumerable<IAction> EnumUndoableActions()
        {
            SimpleHistoryNode Current = Head;
            while (Current != null && Current != CurrentState && Current.NextAction != null)
            {
                yield return Current.NextAction;
                Current = Current.NextNode;
            }
        }

        /// <summary>
        /// Moves the forward.
        /// </summary>
        public void MoveForward()
        {
            if (!CanMoveForward)
            {
                throw new InvalidOperationException(
                    "History.MoveForward() cannot execute because"
                    + " CanMoveForward returned false (the current state"
                    + " is the last state in the undo buffer.");
            }
            CurrentState.NextAction.Execute();
            CurrentState = CurrentState.NextNode;
            Length += 1;
            RaiseUndoBufferChanged();
        }

        /// <summary>
        /// Moves the back.
        /// </summary>
        public void MoveBack()
        {
            if (!CanMoveBack)
            {
                throw new InvalidOperationException(
                    "History.MoveBack() cannot execute because"
                    + " CanMoveBack returned false (the current state"
                    + " is the last state in the undo buffer.");
            }
            CurrentState.PreviousAction.UnExecute();
            CurrentState = CurrentState.PreviousNode;
            Length -= 1;
            RaiseUndoBufferChanged();
        }

        /// <summary>
        /// Gets a value indicating whether this instance can move forward.
        /// </summary>
        /// <value>
        /// 	<c>true</c> if this instance can move forward; otherwise, <c>false</c>.
        /// </value>
        public bool CanMoveForward
        {
            get
            {
                return CurrentState.NextAction != null &&
                    CurrentState.NextNode != null;
            }
        }

        /// <summary>
        /// Gets a value indicating whether this instance can move back.
        /// </summary>
        /// <value>
        /// 	<c>true</c> if this instance can move back; otherwise, <c>false</c>.
        /// </value>
        public bool CanMoveBack
        {
            get
            {
                return CurrentState.PreviousAction != null &&
                        CurrentState.PreviousNode != null;
            }
        }

        /// <summary>
        /// The length of Undo buffer (total number of undoable actions)
        /// </summary>
        public int Length { get; set; }

        public IEnumerator<IAction> GetEnumerator()
        {
            return EnumUndoableActions().GetEnumerator();
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}
