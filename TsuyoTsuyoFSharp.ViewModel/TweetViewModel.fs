namespace TweetViewModel

open System
open System.Diagnostics
open System.Windows
open System.Windows.Input
open System.ComponentModel
open System.Windows.Media.Imaging
open System.Collections.ObjectModel

/// ViewModel
type ViewModel() = 
  let mutable _items = new ObservableCollection<TweetModel.Model>()
  let _propertyChanged = Event<_,_>()
  
  interface INotifyPropertyChanged with
    [<CLIEvent>]
    member self.PropertyChanged = _propertyChanged.Publish

  member this.Items
    with get() = _items
     and set(v) = 
        _items <- new ObservableCollection<TweetModel.Model>(v)
        _propertyChanged.Trigger(this, PropertyChangedEventArgs("Items"))