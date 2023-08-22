[<AutoOpen>]
module Farmer.Builders.Builders_RouteServer

open Farmer
open Farmer.Arm
open Farmer.RouteServer


type RSBGPConnectionConfig =
    {
        Name: string
        PeerIp: string
        PeerAsn: int
        Dependencies: Set<ResourceId>
    }

type RSBGPConnectionBuilder() =
    member _.Yield _ =
        {
            Name = ""
            PeerIp = ""
            PeerAsn = 0
            Dependencies = Set.empty
        }

    [<CustomOperation "name">]
    member _.ConnectionName(state: RSBGPConnectionConfig, name) = { state with Name = name }

    [<CustomOperation "peer_ip">]
    member _.PeerIp(state: RSBGPConnectionConfig, peerIp) = { state with PeerIp = peerIp }

    [<CustomOperation "peer_asn">]
    member _.PeerAsn(state: RSBGPConnectionConfig, peerAsn) = { state with PeerAsn = peerAsn }

    interface IDependable<RSBGPConnectionConfig> with
        /// Adds an explicit dependency to this Container App Environment.
        member _.Add state newDeps =
            { state with
                Dependencies = state.Dependencies + newDeps
            }

let routeServerBGPConnection = RSBGPConnectionBuilder()

type RouteServerConfig =
    {
        Name: ResourceName
        Sku: RouteServer.Sku
        AllowBranchToBranchTraffic: FeatureFlag option
        HubRoutingPreference: HubRoutingPreference option
        BGPConnections: RSBGPConnectionConfig list
        VirtualNetwork: LinkedResource option
        SubnetName: string option
        SubnetPrefix: IPAddressCidr option
        LinkedSubnet: LinkedResource option
        Tags: Map<string, string>
    }

    interface IBuilder with
        member this.ResourceId = routeServers.resourceId this.Name

        member this.BuildResources location =
            [
                let vnetId =
                    this.VirtualNetwork
                    |> Option.defaultWith (fun _ -> raiseFarmer "Must set 'vnet' for route server")

                match this.LinkedSubnet with
                | Some subnet ->
                    //ip configuration
                    {
                        RouteServerIPConfig.Name = ResourceName $"{this.Name.Value}-ipconfig"
                        RouteServer = Managed(routeServers.resourceId this.Name)
                        PublicIpAddress = LinkedResource.Managed(publicIPAddresses.resourceId $"{this.Name.Value}-pip")
                        SubnetId =
                            LinkedResource.Managed(
                                subnets.resourceId (ResourceName vnetId.Name.Value, subnet.Name)
                            )
                    }
                | None ->
                    match this.SubnetName, this.SubnetPrefix with
                    | Some subnetName, Some subnetPrefix ->
                        //subnet
                        {
                            Subnet.Name = ResourceName subnetName
                            Prefix = IPAddressCidr.format subnetPrefix
                            VirtualNetwork = Some(vnetId)
                            NetworkSecurityGroup = None
                            Delegations = []
                            NatGateway = None
                            ServiceEndpoints = []
                            AssociatedServiceEndpointPolicies = []
                            PrivateEndpointNetworkPolicies = None
                            PrivateLinkServiceNetworkPolicies = None
                        }

                        //ip configuration
                        {
                            RouteServerIPConfig.Name = ResourceName $"{this.Name.Value}-ipconfig"
                            RouteServer = Managed(routeServers.resourceId this.Name)
                            PublicIpAddress = LinkedResource.Managed(publicIPAddresses.resourceId $"{this.Name.Value}-pip")
                            SubnetId =
                                LinkedResource.Managed(
                                    subnets.resourceId (ResourceName vnetId.Name.Value, ResourceName subnetName)
                                )
                        }
                    | _ ->
                        raiseFarmer
                            $"subnetName and subnetPrefix must be specified for a new subnet if no existing subnet provided."
                
                //public ip
                {
                    PublicIpAddress.Name = ResourceName $"{this.Name.Value}-pip"
                    AvailabilityZone = None
                    Location = location
                    Sku = PublicIpAddress.Sku.Standard
                    AllocationMethod = PublicIpAddress.AllocationMethod.Static
                    DomainNameLabel = None
                    Tags = this.Tags
                }

                //route server
                {
                    RouteServer.Name = this.Name
                    Location = location
                    Sku = this.Sku
                    AllowBranchToBranchTraffic =
                        this.AllowBranchToBranchTraffic |> Option.defaultValue FeatureFlag.Disabled
                    HubRoutingPreference =
                        this.HubRoutingPreference
                        |> Option.defaultValue HubRoutingPreference.ExpressRoute
                    Tags = this.Tags
                }

                //bgp connections
                for connection in this.BGPConnections do
                    {
                        RouteServerBGPConnection.Name = ResourceName connection.Name
                        RouteServer = Managed(routeServers.resourceId this.Name)
                        PeerIp = connection.PeerIp
                        PeerAsn = connection.PeerAsn
                        IpConfig =
                            LinkedResource.Managed(
                                routeServerIPConfigs.resourceId (
                                    ResourceName this.Name.Value,
                                    ResourceName $"{this.Name.Value}-ipconfig"
                                )
                            )
                        Dependencies = connection.Dependencies
                    }
            ]

type RouteServerBuilder() =
    member _.Yield _ =
        {
            Name = ResourceName.Empty
            Sku = Standard
            AllowBranchToBranchTraffic = None
            HubRoutingPreference = None
            BGPConnections = []
            VirtualNetwork = None
            SubnetName = None
            SubnetPrefix = None
            LinkedSubnet = None
            Tags = Map.empty
        }

    [<CustomOperation "name">]
    member _.Name(state: RouteServerConfig, name: string) = { state with Name = ResourceName name }

    [<CustomOperation "sku">]
    member _.Sku(state: RouteServerConfig, sku) = { state with Sku = sku }

    [<CustomOperation "allow_branch_to_branch_traffic">]
    member _.AllowBranchToBranchTraffic(state: RouteServerConfig, flag: bool) =
        { state with
            AllowBranchToBranchTraffic = Some(FeatureFlag.ofBool flag)
        }

    [<CustomOperation "routing_preference">]
    member _.HubRoutingPreference(state: RouteServerConfig, routingPreference) =
        { state with
            HubRoutingPreference = Some(routingPreference)
        }

    [<CustomOperation "add_bgp_connections">]
    member _.AddIPConfigs(state: RouteServerConfig, connections: RSBGPConnectionConfig list) =
        { state with
            BGPConnections = connections @ state.BGPConnections
        }

     // create subnet through Farmer. Need to specify subnet_name and subnet_prefix
    [<CustomOperation "subnet_name">]
    member _.SubnetName(state: RouteServerConfig, name) = { state with SubnetName = Some(name) }

    [<CustomOperation "subnet_prefix">]
    member _.SubnetPrefix(state: RouteServerConfig, prefix) =
        { state with
            SubnetPrefix = Some(IPAddressCidr.parse prefix)
        }

    // linked to external existing subnet
    [<CustomOperation "link_to_subnet">]
    member _.LinkToSubnet(state: RouteServerConfig, name: string) =
        { state with
            LinkedSubnet = Some(Unmanaged(subnets.resourceId (ResourceName name)))
        }

    // linked to managed vnet created by Farmer and linked by user
    [<CustomOperation "link_to_vnet">]
    member _.LinkToVNetId(state: RouteServerConfig, vnetId: ResourceId) =
        { state with
            VirtualNetwork = Some(Managed vnetId)
        }

    // linked to external existing vnet
    member _.LinkToVNetId(state: RouteServerConfig, vnetName: string) =
        { state with
            VirtualNetwork = Some(Unmanaged(virtualNetworks.resourceId (ResourceName vnetName)))
        }

    interface ITaggable<RouteServerConfig> with
        member _.Add state tags =
            { state with
                Tags = state.Tags |> Map.merge tags
            }

let routeServer = RouteServerBuilder()
