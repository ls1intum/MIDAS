//
//  AppDelegate.m
//  MidasDemo
//
//  Created by Thomas Günzel on 16/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "AppDelegate.h"


// NASA API Key fzeZDwRJNGGqQS7Z2SAmU6xbsLnWvFPP2EMthDVQ


@import Midas;

@interface AppDelegate ()
@property(readwrite,nonatomic,strong) MDWindow *mdWindow;
@end

@implementation AppDelegate

-(UIWindow *)window {
	if(_mdWindow == nil) {
		self.mdWindow = [[MDWindow alloc] initWithFrame:[UIScreen mainScreen].bounds];
		self.mdWindow.tracker = [MDTracker sharedTracker];
	}
	return self.mdWindow;
}

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
	[self.mdWindow.tracker adviseUpload];
	return YES;
}

- (void)applicationDidEnterBackground:(UIApplication *)application {
	[self.mdWindow.tracker endSession];
}

- (void)applicationWillEnterForeground:(UIApplication *)application {
	[self.mdWindow.tracker adviseUpload];
	[self.mdWindow.tracker startSession];
}


- (void)applicationWillResignActive:(UIApplication *)application {
	// Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
	// Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
}


- (void)applicationDidBecomeActive:(UIApplication *)application {
	// Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
}

- (void)applicationWillTerminate:(UIApplication *)application {
	// Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
}

@end
