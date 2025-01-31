#!/bin/bash
adb shell pm clear com.google.android.webview
adb shell pm clear com.android.chrome
adb shell pm uninstall --user 0 com.google.android.webview
adb shell cmd package install-existing com.google.android.webview
