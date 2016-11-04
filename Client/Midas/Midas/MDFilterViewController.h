//
//  MDFilterViewController.h
//  Midas
//
//  Created by Thomas Günzel on 08/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDVerticalViewController.h"

@class MDFilter;

@interface MDFilterViewController : MDVerticalViewController

-(instancetype)initWithFilter:(MDFilter*)filter;

-(void)back:(id)sender;
-(void)reset:(id)sender;

@property(readwrite,nonatomic,strong) NSString *filterName;
@property(readonly,nonatomic,strong) MDFilter *filter;

@end
