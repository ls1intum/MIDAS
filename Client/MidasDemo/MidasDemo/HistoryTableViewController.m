//
//  HistoryTableViewController.m
//  MidasDemo
//
//  Created by Thomas Günzel on 27/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "HistoryTableViewController.h"
#import "PictureViewController.h"

@import Midas;

@interface HistoryTableViewController ()

@property(readwrite,nonatomic,strong) NSArray *pictures;
@property (weak, nonatomic) IBOutlet UIBarButtonItem *settingsButton;

@end

@implementation HistoryTableViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Uncomment the following line to preserve selection between presentations.
    // self.clearsSelectionOnViewWillAppear = NO;
	//[self performSelector:@selector(testSegue) withObject:nil afterDelay:2.0];
    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    // self.navigationItem.rightBarButtonItem = self.editButtonItem;
	NSData *data = [NSData dataWithContentsOfFile:[[NSBundle mainBundle] pathForResource:@"APOD" ofType:@"json"]];
	NSArray *pictures = [NSJSONSerialization JSONObjectWithData:data options:0 error:nil];
	_pictures = pictures;
	[self.tableView reloadData];
}

-(void)viewDidAppear:(BOOL)animated {
	[super viewDidAppear:animated];
	NSLog(@"Appeared");
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return _pictures.count;
}


- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"pictureCell" forIndexPath:indexPath];
    
    // Configure the cell...
	NSDictionary *picture = [_pictures objectAtIndex:indexPath.row];
	cell.textLabel.text = [picture objectForKey:@"title"];
	cell.detailTextLabel.text = [picture objectForKey:@"date"];
    
    return cell;
}


#pragma mark - Navigation
- (IBAction)emptySelector:(id)sender {
	NSLog(@"Did press");
}

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
	[super prepareForSegue:segue sender:sender];
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
	if([segue.identifier isEqualToString:@"showPicture"]) {
		PictureViewController *destination = (PictureViewController*)segue.destinationViewController;
		NSDictionary *picture = [_pictures objectAtIndex:self.tableView.indexPathForSelectedRow.row];
		destination.imageJsonDictionary = picture;
		//[[MDTracker sharedTracker] trackAction:NSStringFromSelector(@selector(showPicture:)) onViewController:[self md_identifier] sender:@"view.someButton"];
	}
}

-(void)showPicture:(id)sender {
	
}

-(void)testSegue {
	[self md_simulateSegue:@"showPicture" sender:nil];
}

@end
