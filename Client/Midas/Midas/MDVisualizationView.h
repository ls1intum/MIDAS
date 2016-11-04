//
//  MDVisualizationView.h
//  Midas
//
//  Created by Thomas Günzel on 01/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface MDVisualizationView : UIView


@property(readwrite,nonatomic,strong) UIView *underlyingView;
@property(readwrite,nonatomic,strong) UIView *overlayView;

@end
