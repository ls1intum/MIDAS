//
//  MDPercentageOverlayView.m
//  Midas
//
//  Created by Thomas Günzel on 15/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDPercentageOverlayView.h"
#import "MDRecordedAction.h"

#import "MDPercentLabel.h"

@interface MDPercentageOverlayView()

@property(readwrite,nonatomic,strong) NSMutableDictionary<NSString*,MDPercentLabel*> *senderLabelMapping;

@end

@implementation MDPercentageOverlayView

-(void)setActions:(NSArray<MDRecordedAction *> *)actions {
	if(_actions == actions) {
		return;
	}
	
	_actions = actions;
	[self updatePercentages];
}

-(void)setUnderlyingViewController:(UIViewController *)underlyingViewController {
	if(_underlyingViewController == underlyingViewController) {
		return;
	}
	
	_underlyingViewController = underlyingViewController;
	_senderLabelMapping = [[NSMutableDictionary alloc] init];
}


-(void)updatePercentages {
	for (MDPercentLabel *lbl in _senderLabelMapping.objectEnumerator) {
		lbl.actionsCount = 0;
		lbl.allActionsCount = _actions.count;
	}
	
	for (MDRecordedAction *action in _actions) {
		NSString *sender = action.senderKeyPath;
		MDPercentLabel *lbl = [self labelForSender:sender];
		if(lbl) {
			lbl.actionsCount++;
		}
	}
	
	for (MDPercentLabel *lbl in _senderLabelMapping.objectEnumerator) {
		[lbl updateLabel];
	}
}

-(MDPercentLabel*)labelForSender:(NSString*)sender {
	MDPercentLabel *lbl = [_senderLabelMapping objectForKey:sender];
	if(lbl) {
		return lbl;
	}
	
	@try {
		UIView *senderView = [_underlyingViewController valueForKeyPath:sender];
		if([senderView isKindOfClass:[UIView class]]) {
			CGRect frame = senderView.frame;
//			frame = [_underlyingViewController.view convertRect:frame fromView:senderView];
			lbl = [[MDPercentLabel alloc] initWithFrame:frame];
			lbl.allActionsCount = _actions.count;
			[_senderLabelMapping setObject:lbl forKey:sender];
			[self addSubview:lbl];
		}
	} @catch (NSException *exception) {
		NSLog(@"Error creating label: %@",exception);
	}
	
	return lbl;
}

@end
