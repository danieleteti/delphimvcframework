Steps to host DMVC ISAPI dll on IIS 10 (tested on IIS 10, but should be applicable on lower versions as well)

1. Go to **Start** , type "Turn Windows" in the search field. Then select **Turn Windows features on or off** from the matching entries.
2. Expand the **Internet Information Services** node of the **Windows Features** dialog box and then:
   - In  **Web Management Tools**, check  **IIS Management Console**.
   - In the **World Wide Web Services**,expand the  **Application Development Features**  and then check  **CGI**  and  **ISAPI Extensions**:

      ![Turn windows feature](http://docwiki.embarcadero.com/images/RADStudio/Rio/e/6/62/WindowsFeatures.png)
3. After the Windows features are enabled, go to **Start**, type "IIS" in the search field, and open **Internet Information Services (IIS) Manager**. Expand the **Connections** nodes, click **Application Pools** and then select **DefaultAppPool**:

   ![Enable 32 bit application](http://docwiki.embarcadero.com/images/RADStudio/Rio/e/6/68/Enable32bit.png)
4. In the **Actions** panel, click **Advanced Settings** and set **Enable 32-Bit Applications** to **True**:

   ![Enable 32 bits](http://docwiki.embarcadero.com/images/RADStudio/Rio/e/9/96/AppPoolSett.png)
 
5. Expand the Sites node from the **Connections** panel, right-click **Default Web Site** and select **Add Virtual Directory**. Enter an alias and a physical path:
    
    ![Add virtual directory](http://docwiki.embarcadero.com/images/RADStudio/Rio/e/f/fc/AddVirtualDir.png)
    
6. Go to the root node from the **Connections** panel and double-click **ISAPI and CGI Restrictions**. Select **Edit Features Settings** from the **Actions** panel and check the two options:
    
    ![Edit feature settings](http://docwiki.embarcadero.com/images/RADStudio/Rio/e/f/f2/ISAPI&CGI.png)
    
7. Return to the root node and double-click Handler Mappings. Go to the **Actions** panel, select Edit Feature Permissions and check **Execute**.

    ![Execute  permission](http://docwiki.embarcadero.com/images/RADStudio/Rio/e/f/f8/EditFeaturesPerm.png)
 
8. Select the root node, and from the **Actions** panel click **Start** to start the server.

> References :
> [http://docwiki.embarcadero.com/RADStudio/Rio/en/Tutorial:_DataSnap_Application_Using_an_ISAPI_DLL_Server?fbclid=IwAR0bHi-RJEHQoE4_vS6c5d1gfQRbzP-jM3h9qpGfem0ghCLPRVWV_lSnnS8](http://docwiki.embarcadero.com/RADStudio/Rio/en/Tutorial:_DataSnap_Application_Using_an_ISAPI_DLL_Server?fbclid=IwAR0bHi-RJEHQoE4_vS6c5d1gfQRbzP-jM3h9qpGfem0ghCLPRVWV_lSnnS8) 
