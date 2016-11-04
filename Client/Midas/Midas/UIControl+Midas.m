//
//  UIControl+Midas.m
//  Midas
//
//  Created by Thomas Günzel on 18/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "UIControl+MidasInternal.h"
#import "UIViewController+MidasInternal.h"
#import "MDViewController.h"
#import "MDTracker.h"

@import ObjectiveC.runtime;

static IMP __original_send_Method_Imp;
static BOOL __send_Swizzled = NO;
void _replacement_send_Method(id self, SEL _cmd, SEL action, id target, UIEvent* event) {
	((void(*)(id,SEL,SEL,id,UIEvent*))__original_send_Method_Imp)(self, _cmd, action, target, event);
	[self md_sendAction:action to:target forEvent:event];
}


@implementation UIControl (Midas)


+(void)md_swizzleSend {
	if(__send_Swizzled == NO) {
		Method original = class_getInstanceMethod([self class], @selector(sendAction:to:forEvent:));
		__original_send_Method_Imp = method_setImplementation(original, (IMP)_replacement_send_Method);
		__send_Swizzled = YES;
	}
}

-(void)md_sendAction:(SEL)action to:(id)target forEvent:(UIEvent*)event {
	id sender = self;
	if([target isKindOfClass:[UIViewController class]] == NO) {
		if([target isKindOfClass:[UIBarButtonItem class]]) {
			UIResponder *navCon = self;
			while ([navCon isKindOfClass:[UINavigationController class]] == NO && navCon != nil) {
				navCon = navCon.nextResponder;
			}
			if(navCon != nil) {
				UINavigationController *nav = (UINavigationController*)navCon;
				sender = target;
				target = [nav.viewControllers lastObject];
			} else {
				return;
			}
		} else {
			return;
		}
	}
	if([target md_trackViewController] == NO) {
		return;
	}
	
	NSString *identifier = [target md_identifier];
	if(identifier == nil) {
		return;
	}
	
	UIViewController *vc = target;
	
	NSString *key = [vc md_keyForSender:sender];
	if(key != nil) {
		NSLog(@"I'm linked to %@.%@",NSStringFromClass([target class]),key);
		MDTracker *tracker = [target md_tracker];
		[tracker trackAction:NSStringFromSelector(action) onViewController:identifier sender:key];
	}
}

@end
