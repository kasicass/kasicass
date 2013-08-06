using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.CalculateInHex
{
	public class RoundedButton : Control
	{
		RoundedButtonDecorator decorator;
		public static readonly RoutedEvent ClickEvent;

		static RoundedButton()
		{
			ClickEvent = EventManager.RegisterRoutedEvent("Click", RoutingStrategy.Bubble,
				typeof(RoutedEventHandler), typeof(RoundedButton));
		}

		public RoundedButton()
		{
			decorator = new RoundedButtonDecorator();
			AddVisualChild(decorator);
			AddLogicalChild(decorator);
		}

		public UIElement Child
		{
			set { decorator.Child = value; }
			get { return decorator.Child; }
		}

		public bool IsPressed
		{
			set { decorator.IsPressed = value; }
			get { return decorator.IsPressed; }
		}

		public event RoutedEventHandler Click
		{
			add { AddHandler(ClickEvent, value); }
			remove { RemoveHandler(ClickEvent, value); }
		}

		protected override int VisualChildrenCount
		{
			get { return 1; }
		}

		protected override Visual GetVisualChild(int index)
		{
			if (index > 0)
				throw new ArgumentOutOfRangeException("index");

			return decorator;
		}

		protected override Size MeasureOverride(Size sizeAvailable)
		{
			decorator.Measure(sizeAvailable);
			return decorator.DesiredSize;
		}

		protected override Size ArrangeOverride(Size sizeArrange)
		{
			decorator.Arrange(new Rect(new Point(0,0), sizeArrange));
			return sizeArrange;
		}

		protected override void OnMouseMove(MouseEventArgs args)
		{
			base.OnMouseMove(args);
			if (IsMouseCaptured)
				IsPressed = IsMouseReallyOver;
		}

		protected override void OnMouseLeftButtonDown(MouseButtonEventArgs args)
		{
			base.OnMouseLeftButtonDown(args);
			CaptureMouse();
			IsPressed = true;	
			args.Handled = true;
		}

		protected override void OnMouseLeftButtonUp(MouseButtonEventArgs args)
		{
			base.OnMouseLeftButtonUp(args);
			if (IsMouseCaptured)
			{
				if (IsMouseReallyOver)
					OnClick();

				Mouse.Capture(null);
				IsPressed = false;
				args.Handled = true;
			}
		}

		bool IsMouseReallyOver
		{
			get
			{
				Point pt = Mouse.GetPosition(this);
				return (pt.X >= 0 && pt.X < ActualWidth && pt.Y >= 0 && pt.Y < ActualHeight);
			}
		}

		protected virtual void OnClick()
		{
			RoutedEventArgs argsEvent = new RoutedEventArgs();
			argsEvent.RoutedEvent = RoundedButton.ClickEvent;
			argsEvent.Source = this;
			RaiseEvent(argsEvent);
		}
	}
}

