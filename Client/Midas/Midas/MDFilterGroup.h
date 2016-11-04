//
//  MDFilterGroup.h
//  Midas
//
//  Created by Thomas Günzel on 08/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

@import UIKit;

#import "MDFilter.h"

@interface MDFilterGroup : MDFilter

-(instancetype)initRootFilterGroup;


@property(readwrite,nonatomic,strong) NSArray<MDFilter*> *filters;

@end
