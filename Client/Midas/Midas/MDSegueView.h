//
//  MDSegueView.h
//  Midas
//
//  Created by Thomas Günzel on 16/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface MDSegueView : UIView

@property(readwrite,nonatomic,strong) NSString *rootViewIdentifier;
@property(readwrite,nonatomic,strong) NSMutableDictionary *connections;
@property(readwrite,nonatomic,strong) NSDictionary *weightedConnections;

@end
