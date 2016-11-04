//
//  MDWindow.h
//  Midas
//
//  Created by Thomas Günzel on 16/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

@import UIKit;

@class MDTracker;

@interface MDWindow : UIWindow

@property(readwrite,nonatomic,strong) MDTracker *tracker;


@property(readwrite,nonatomic) BOOL visualizingMode;
@end
