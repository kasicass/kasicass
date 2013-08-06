using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.BuildButtonFactory
{
	public class BuildButtonFactory : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new BuildButtonFactory());
		}

		public BuildButtonFactory()
		{
			Title = "Build Button Factory";

			ControlTemplate template = new ControlTemplate(typeof(Button));

			FrameworkElementFactory factoryBorder = new FrameworkElementFactory(typeof(Border));
			factoryBorder.Name = "border";
			factoryBorder.SetValue(Border.BorderBrushProperty, Brushes.Red);
			factoryBorder.SetValue(Border.BorderThicknessProperty, new Thickness(3));
			factoryBorder.SetValue(Border.BackgroundProperty, SystemColors.ControlLightBrush);

			FrameworkElementFactory factoryContent = new FrameworkElementFactory(typeof(ContentPresenter));
			factoryContent.Name = "content";
			factoryContent.SetValue(ContentPresenter.ContentProperty,
				new TemplateBindingExtension(Button.ContentProperty));
			factoryContent.SetValue(ContentPresenter.MarginProperty,
				new TemplateBindingExtension(Button.PaddingProperty));

			factoryBorder.AppendChild(factoryContent);
			template.VisualTree = factoryBorder;

			Trigger trig = new Trigger();
			trig.Property = UIElement.IsMouseOverProperty;
			trig.Value = true;

			Setter set = new Setter();
			set.Property = Border.CornerRadiusProperty;
			set.Value = new CornerRadius(24);
			set.TargetName = "border";
			trig.Setters.Add(set);

			set = new Setter();
			set.Property = Control.FontStyleProperty;
			set.Value = FontStyles.Italic;
			trig.Setters.Add(set);

			template.Triggers.Add(trig);

			Button btn = new Button();
			btn.Template = template;
			btn.Content = "Button with Custom Template";
			btn.Padding = new Thickness(20);
			btn.FontSize = 48;
			btn.HorizontalAlignment = HorizontalAlignment.Center;
			btn.VerticalAlignment = VerticalAlignment.Center;
			btn.Click += ButtonOnClick;

			Content = btn;
		}

		void ButtonOnClick(object sender, RoutedEventArgs args)
		{
			MessageBox.Show("You clicked the button", Title);
		}
	}
}

