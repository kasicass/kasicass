namespace Illusion
{
    /// <summary>
    /// Represents a node of the doubly linked-list SimpleHistory
    /// (StateX in the following diagram:)
    /// (State0) --- [Action0] --- (State1) --- [Action1] --- (State2)
    /// StateX (e.g. State1) has a link to the previous State, previous Action,
    /// next State and next Action.
    /// As you move from State1 to State2, an Action1 is executed (Redo).
    /// As you move from State1 to State0, an Action0 is un-executed (Undo).
    /// </summary>
    internal class SimpleHistoryNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="SimpleHistoryNode"/> class.
        /// </summary>
        /// <param name="lastExistingAction">The last existing action.</param>
        /// <param name="lastExistingState">Last state of the existing.</param>
        public SimpleHistoryNode(IAction lastExistingAction, SimpleHistoryNode lastExistingState)
        {
            PreviousAction = lastExistingAction;
            PreviousNode = lastExistingState;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SimpleHistoryNode"/> class.
        /// </summary>
        public SimpleHistoryNode()
        {
        }

        public IAction PreviousAction { get; set; }
        public IAction NextAction { get; set; }
        public SimpleHistoryNode PreviousNode { get; set; }
        public SimpleHistoryNode NextNode { get; set; }
    }
}
