//
//  UIViewController+Midas.m
//  Midas
//
//  Created by Thomas Günzel on 17/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "UIViewController+MidasInternal.h"

#import "MDTracker.h"

@import ObjectiveC.runtime;

#import <objc/runtime.h>

static IMP __original_viewDidAppear_Method_Imp;
static BOOL __viewDidAppear_Swizzled = NO;
void _replacement_viewDidAppear_Method(id self, SEL _cmd, BOOL animated) {
	((void(*)(id,SEL,BOOL))__original_viewDidAppear_Method_Imp)(self, _cmd, animated);
	[self md_track];
}

static IMP __original_prepareForSegue_Method_Imp;
static BOOL __prepareForSegue_Swizzled = NO;
void _replacement_prepareForSegue_Method(id self, SEL _cmd, UIStoryboardSegue *segue, id sender) {
	((void(*)(id,SEL,UIStoryboardSegue*,id))__original_prepareForSegue_Method_Imp)(self, _cmd, segue, sender);
	[self md_logPrepareForSegue:segue sender:sender];
}

@implementation UIViewController (MidasInternal)

+(void)md_swizzle {
	[self md_swizzleViewDidAppear];
	[self md_swizzlePrepareForSegue];
}

+(void)md_swizzleViewDidAppear {
	if(__viewDidAppear_Swizzled == NO) {
		Method original = class_getInstanceMethod([self class], @selector(viewDidAppear:));
		__original_viewDidAppear_Method_Imp = method_setImplementation(original, (IMP)_replacement_viewDidAppear_Method);
		__viewDidAppear_Swizzled = YES;
	}
}

+(void)md_swizzlePrepareForSegue {
	if(__prepareForSegue_Swizzled == NO) {
		Method original = class_getInstanceMethod([self class], @selector(prepareForSegue:sender:));
		__original_prepareForSegue_Method_Imp = method_setImplementation(original, (IMP)_replacement_prepareForSegue_Method);
		__prepareForSegue_Swizzled = YES;
	}
}

-(void)md_logPrepareForSegue:(UIStoryboardSegue*)segue sender:(id)sender {
	NSString *key = [self md_keyForSender:sender];
	if(key == nil && [sender isKindOfClass:[UITableViewCell class]]) {
		UITableViewCell *senderCell = (UITableViewCell*)sender;
		id view = [senderCell superview];
		
		while (view && [view isKindOfClass:[UITableView class]] == NO) {
			view = [view superview];
		}
		
		UITableView *tableView = (UITableView *)view;
		if([tableView isKindOfClass:[UITableView class]]) {
			key = [self md_keyForSender:tableView];
		}
	}
	[[self md_tracker] trackSegue:segue sender:key];
}

@end

@implementation UIViewController (Midas)

-(NSString *)md_identifier {
	if([self respondsToSelector:@selector(md_excludeTitleFromIdentifier)] && [self md_excludeTitleFromIdentifier] == NO) {
		NSString *title = self.title;
		if(title) {
			return [NSString stringWithFormat:@"%@[%@]",NSStringFromClass(self.class),title];
		}
	}
	return [NSString stringWithFormat:@"%@",NSStringFromClass(self.class)];
}

-(BOOL)md_excludeTitleFromIdentifier {
	return NO;
}

-(BOOL)md_trackViewController {
	NSString *classname = NSStringFromClass([self class]);
	return [classname hasPrefix:@"UI"] == NO;
}

-(void)md_track {
	if([self respondsToSelector:@selector(md_trackViewController)] && [self md_trackViewController] == YES) {
		[self md_tracker].currentViewController = [self md_identifier];
	}
}

-(MDTracker*)md_tracker {
	return [MDTracker sharedTracker];
}

-(NSString *)md_keyForSender:(id)sender inClass:(Class)targetClass {
//	Class targetClass = [self class];
	if([targetClass isSubclassOfClass:[UIViewController class]] == NO || targetClass == [UIViewController class]) {
		return nil;
	}
	unsigned int count = 0;
	objc_property_t *properties = class_copyPropertyList(targetClass, &count);
	
	for (unsigned int i = 0; i < count; i++) {
		objc_property_t property = properties[i];
		const char *name = property_getName(property);
		
		NSString *key = [NSString stringWithUTF8String:name];
		id value = nil;
		@try {
			value = [self valueForKey:key];
		} @catch (NSException *exception) {
			NSLog(@"Key not known %@ on %@",key, self);
		}
		if(value == sender) {
			return key;
		}
	}
	return [self md_keyForSender:sender inClass:[targetClass superclass]];
}

-(NSString *)md_keyForSender:(id)sender {
	Class targetClass = [self class];
	return [self md_keyForSender:sender inClass:targetClass];
}

-(void)md_simulateSegue:(NSString *)segueName sender:(NSString*)senderKey {
	id sender = nil;
	if(senderKey != nil) {
		@try {
			sender = [self valueForKey:senderKey];
		} @catch (NSException *exception) {
			NSLog(@"Couldn't get sender %@ on %@",senderKey,self);
		}
	}
	[self performSegueWithIdentifier:segueName sender:sender];
}

-(UIViewController *)md_namedViewController {
	if(self.md_trackViewController == YES) {
		return self;
	}
	
	if ([self isKindOfClass:[UITabBarController class]]) {
		UITabBarController* tabBarController = (UITabBarController*)self;
		return tabBarController.selectedViewController.topmostViewController;
	} else if ([self isKindOfClass:[UINavigationController class]]) {
		UINavigationController* navigationController = (UINavigationController*)self;
		return navigationController.visibleViewController.topmostViewController;
	} else if (self.presentedViewController) {
		UIViewController* presentedViewController = self.presentedViewController;
		return presentedViewController.topmostViewController;
	} else {
		return self;
	}
}

-(UIViewController *)topmostViewController {
	if (self.presentedViewController) {
		UIViewController* presentedViewController = self.presentedViewController;
		return presentedViewController.topmostViewController;
	} else {
		return self;
	}
}

@end
