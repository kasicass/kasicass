using System.Reflection;
using System.ComponentModel;

namespace Illusion
{
    /// <summary>
    /// This is a concrete <see cref="AbstractAction"/> that can change any property on any object
    /// It can also undo what it did
    /// </summary>
    public class SetPropertyAction : AbstractAction
    {
        #region Constructor

        /// <summary>
        /// Initializes a new instance of the <see cref="SetPropertyAction"/> class.
        /// </summary>
        /// <param name="parentObject">The parent object.</param>
        /// <param name="propertyName">Name of the property.</param>
        /// <param name="value">The value.</param>
        public SetPropertyAction(object parentObject, string propertyName, object value)
        {
            ParentObject = parentObject;
            Property = parentObject.GetType().GetProperty(propertyName);
            Value = value;
        }

        public SetPropertyAction(object parentObject, string propertyName, object value, object oldValue)
            : this(parentObject, propertyName, value)
        {
            OldValue = oldValue;
        }

        #endregion

        #region Property

        /// <summary>
        /// Gets or sets the parent object.
        /// </summary>
        /// <value>The parent object.</value>
        public object ParentObject { get; set; }
        /// <summary>
        /// Gets or sets the property.
        /// </summary>
        /// <value>The property.</value>
        public PropertyInfo Property { get; set; }
        /// <summary>
        /// Gets or sets the value.
        /// </summary>
        /// <value>The value.</value>
        public object Value { get; set; }
        /// <summary>
        /// Gets or sets the old value.
        /// </summary>
        /// <value>The old value.</value>
        public object OldValue { get; set; }

        #endregion

        #region Method

        /// <summary>
        /// Override execute core to provide your logic that actually performs the action
        /// </summary>
        protected override void ExecuteCore()
        {
            if (OldValue == null)
            {
                OldValue = Property.GetValue(ParentObject, null);
            }

            IEditableObject edit = ParentObject as IEditableObject;
            if (edit != null)
            {
                edit.BeginEdit();
            }
            Property.SetValue(ParentObject, Value, null);
            if (edit != null)
            {
                edit.EndEdit();
            }
        }

        /// <summary>
        /// Override this to provide the logic that undoes the action
        /// </summary>
        protected override void UnExecuteCore()
        {
            IEditableObject edit = ParentObject as IEditableObject;
            if(edit != null)
            {
                edit.BeginEdit();
            }
            Property.SetValue(ParentObject, OldValue, null);
            if (edit != null)
            {
                edit.EndEdit();
            }
        }

        /// <summary>
        /// Subsequent changes of the same property on the same object are consolidated into one action
        /// </summary>
        /// <param name="followingAction">Subsequent action that is being recorded</param>
        /// <returns>true if it agreed to merge with the next action, 
        /// false if the next action should be recorded separately</returns>
        public override bool TryToMerge(IAction followingAction)
        {
            SetPropertyAction next = followingAction as SetPropertyAction;
            if (next != null && next.ParentObject == this.ParentObject && next.Property == this.Property)
            {
                Value = next.Value;
                Property.SetValue(ParentObject, Value, null);
                return true;
            }
            return false;
        }

        #endregion
    }
}
