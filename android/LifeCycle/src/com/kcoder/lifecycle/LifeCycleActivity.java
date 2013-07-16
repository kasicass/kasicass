package com.kcoder.lifecycle;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;

public class LifeCycleActivity extends Activity
{
	/** Called when the activity is first created. */
	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);
		Log.d("LifeCycleActivity", "onCreate");
	}

	@Override
	public void onStart()
	{
		super.onStart();
		Log.d("LifeCycleActivity", "onStart");
	}

	@Override
	public void onRestart()
	{
		super.onRestart();
		Log.d("LifeCycleActivity", "onRestart");
	}

	@Override
	public void onResume()
	{
		super.onResume();
		Log.d("LifeCycleActivity", "onResume");
	}

	@Override
	public void onPause()
	{
		super.onPause();
		Log.d("LifeCycleActivity", "onPause");
	}

	@Override
	public void onStop()
	{
		super.onStop();
		Log.d("LifeCycleActivity", "onStop");
	}

	@Override
	public void onDestroy()
	{
		super.onDestroy();
		Log.d("LifeCycleActivity", "onDestroy");
	}
}

