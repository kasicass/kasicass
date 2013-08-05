using System;
using System.Windows;
using System.Windows.Controls;
using System.Winodws.Input;
using System.Windows.Media;

namespace Petzold.EncloseElementInEllipse
{
	public class EllipseWithChild : Petzold.RenderTheBetterEllipse.BetterEllipse
	{
		UIElement child;

		public UIElement Child
		{
			set
			{
				if (child != null)
				{
					RemoveVisualChild(child);
					RemoveLogicalChild(child);
				}

				if ((child = value) != null)
				{
					AddVisualChild(child);
					AddLogicalChild(child);
				}
			}
			get
			{
				return child;
			}
		}

		protected override int VisualChildrenCount
		{
			get
			{
				return Child != null ? 1 : 0;
			}
		}

		protected override Visual GetVisualChild(int index)
		{
			
		}
	}
}

