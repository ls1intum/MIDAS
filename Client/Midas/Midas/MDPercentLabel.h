//
//  MDPercentLabel.h
//  Midas
//
//  Created by Thomas Günzel on 15/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface MDPercentLabel : UILabel

@property(readwrite,nonatomic) NSUInteger actionsCount;
@property(readwrite,nonatomic) NSUInteger allActionsCount;

-(void)updateLabel;

@end
