//
//  MDSegueViewController.m
//  Midas
//
//  Created by Thomas Günzel on 16/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDSegueViewController.h"

#import "MDDownloadedSegues.h"
#import "MDRecordedSegue.h"

#import "MDConnectionCount.h"

#import "MDSegueView.h"

#import "MDFunnel.h"

@interface MDSegueViewController ()<MDDownloadedRecordingsDelegate,MDFunnelDelegate>

@property(readwrite,nonatomic,strong) MDDownloadedSegues *downloadedSegues;

@property(readwrite,nonatomic,strong) CALayer *maskLayer;
@property(readwrite,nonatomic,strong) UIScrollView *scrollView;
@property(readwrite,nonatomic,strong) MDSegueView *segueView;

@property(readwrite,nonatomic,strong) NSMutableDictionary *weightedConnections;

@end

@implementation MDSegueViewController

- (void)viewDidLoad {
    [super viewDidLoad];
	
	_weightedConnections = [[NSMutableDictionary alloc] init];
	
	_initialViewController = @"HistoryTableViewController";
    // Do any additional setup after loading the view.
	_downloadedSegues = [[MDDownloadedSegues alloc] init];
	_downloadedSegues.delegate = self;
	[_downloadedSegues load];
	
//	self.view.backgroundColor = [UIColor redColor];
	[self setupMask];
	[self setupScrollView];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

-(void)dealloc {
	[_segueView removeObserver:self forKeyPath:@"frame"];
}

-(void)setupMask {
	_maskLayer = [CALayer layer];
	_maskLayer.masksToBounds = NO;
	_maskLayer.shadowOffset = CGSizeZero;
	_maskLayer.shadowRadius = 10.0;
	_maskLayer.shadowOpacity = 1.0;
	self.view.layer.mask = _maskLayer;
}

-(void)setupScrollView {
	_scrollView = [[UIScrollView alloc] init];
	_scrollView.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
	_scrollView.frame = self.view.bounds;
	_scrollView.bounces = YES;
	_scrollView.alwaysBounceVertical = YES;
	_scrollView.alwaysBounceHorizontal = YES;

	[self.view addSubview:_scrollView];
	[self setupSegueView];
}

-(void)setupSegueView {
	_segueView = [[MDSegueView alloc] initWithFrame:_scrollView.bounds];
	_segueView.rootViewIdentifier = _initialViewController;
	[_segueView addObserver:self forKeyPath:@"frame" options:(NSKeyValueObservingOptionNew | NSKeyValueObservingOptionInitial) context:nil];
	[_scrollView addSubview:_segueView];
}

-(void)observeValueForKeyPath:(NSString *)keyPath ofObject:(id)object change:(NSDictionary<NSString *,id> *)change context:(void *)context {
	if([keyPath isEqualToString:@"frame"]) {
		_scrollView.contentSize = _segueView.frame.size;
		return;
	}
	[super observeValueForKeyPath:keyPath ofObject:object change:change context:context];
}

-(void)viewDidLayoutSubviews {
	[super viewDidLayoutSubviews];
	CGRect maskFrame = self.view.bounds;
	maskFrame.origin.x += 20.0;
	maskFrame.origin.y += 20.0;
	maskFrame.size.width -= 40.0;
	maskFrame.size.height -= 40.0;
	
	_maskLayer.frame = self.view.bounds;
	_maskLayer.shadowPath = [UIBezierPath bezierPathWithRoundedRect:maskFrame cornerRadius:10.0].CGPath;
}

-(void)recordings:(NSArray *)allRecordings didDownload:(id)downloader {
	NSMutableDictionary *allConnections = [[NSMutableDictionary alloc] init];
	
	
	for (MDRecordedSegue *segue in allRecordings) {
		NSString *f = segue.sourceViewController;
		NSString *t = segue.destinationViewController;
		
		NSMutableSet *fromConn = [allConnections valueForKey:f];
		if(fromConn == nil) {
			fromConn = [[NSMutableSet alloc] init];
			[allConnections setValue:fromConn forKey:f];
		}
		
		
		[fromConn addObject:t];
	}
	
	_segueView.connections = allConnections;
	if(_active) {
		_funnel.input = allRecordings;
		[self generateWeighted:[_funnel filteredForFilterName:_currentFilter] forFilter:_currentFilter];

	}
//	_segueView.weightedConnections = weighted;
	
}


-(void)setActive:(BOOL)active {
	if(_active == active) {
		return;
	}
	
	_active = active;
	
	if(active) {
		_funnel.delegate = self;
		_funnel.visualizationType = MDVisualizationTypeSegues;
		_funnel.input = _downloadedSegues.allRecordings;
		
		[self updateWeighted];
	}
	
}

-(void)funnel:(MDFunnel *)funnel changedFiltered:(NSArray *)filtered forFilterName:(NSString *)filterName {
	if(filterName == nil) {
		filterName = @"";
	}
	
	[self generateWeighted:filtered forFilter:filterName];
	if(_currentFilter == filterName) {
		_segueView.weightedConnections = [_weightedConnections valueForKey:_currentFilter];
	}
}

-(void)funnel:(MDFunnel *)funnel changedImage:(UIImage *)newImage forFilterName:(NSString *)filterName {
	
}


-(NSDictionary*)generateWeighted:(NSArray*)segues forFilter:(NSString*)filterName {
	NSMutableDictionary *weighted = [[NSMutableDictionary alloc] init];
	NSUInteger total = segues.count;

	for (MDRecordedSegue *segue in segues) {
		NSString *f = segue.sourceViewController;
		NSString *t = segue.destinationViewController;

		NSString *key = [NSString stringWithFormat:@"%@ %@",f,t];
		MDConnectionCount *c = [weighted valueForKey:key];
		if(c == nil) {
			c = [[MDConnectionCount alloc] init];
			c.all = total;
			[weighted setValue:c forKey:key];
		}
		
		c.count = c.count + 1;

	}
	[_weightedConnections setValue:weighted forKey:(filterName ? filterName : @"")];
	return weighted;
}

-(void)setCurrentFilter:(NSString *)currentFilter {
	if(_currentFilter == currentFilter) {
		return;
	}
	if(currentFilter == nil) {
		currentFilter = @"";
	}
	
	_currentFilter = currentFilter;
	
	if(_active == NO) {
		return;
	}
	
	[self updateWeighted];
}

-(void)updateWeighted {
	NSDictionary *w = [_weightedConnections valueForKey:_currentFilter];
	if([w count] == 0) {
		[self generateWeighted:[_funnel filteredForFilterName:_currentFilter] forFilter:_currentFilter];
	}
	_segueView.weightedConnections = [_weightedConnections valueForKey:_currentFilter];
}

/*
#pragma mark - Navigation

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
}
*/

@end
