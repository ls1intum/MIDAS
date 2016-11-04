//
//  MDFilterBubblesViewController.h
//  MidasVisualizeUI
//
//  Created by Thomas Günzel on 02/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

@class MDFilter;

@interface MDFilterBubblesViewController : UIViewController

-(instancetype)initWithFilter:(MDFilter*)filter;

@property(readonly,nonatomic,strong) MDFilter *filter;

@property(readwrite,nonatomic,strong) NSString *filterName;


-(void)reset;
@end
