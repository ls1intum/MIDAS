//
//  MDDownloadedSegues.h
//  Midas
//
//  Created by Thomas Günzel on 16/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDDownloadedRecordings.h"

@class MDRecordedSegue;

@interface MDDownloadedSegues : MDDownloadedRecordings

@property(readwrite,nonatomic) NSArray<MDRecordedSegue*> *allRecordings;

@end
