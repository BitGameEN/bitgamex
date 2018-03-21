using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CameraController : MonoBehaviour
{


    // Script attached to the camera to follow player object


    public Transform target; //reference to the player object in the scene
    public float smoothing; //dampening effect for smoother camera movement

    Vector3 offset;



    // Use this for initialization
    void Start()
    {
        offset = transform.position - target.position;

    }

    // Update is called once per frame
    void FixedUpdate()
    {
        Vector3 targetCamPos = target.position + offset;
        targetCamPos.x = 0;
        //this makes the camera follow the target with some smoothing (offset)
        transform.position = Vector3.Lerp(transform.position, targetCamPos, smoothing * Time.deltaTime);

    }
}
































    /*public playerController thePlayer;

    private Vector3 lastPlayerPosition;
    private float distanceToMove;
    private float xdistance, ydistance;

    // Use this for initialization
    void Start()
    {
        thePlayer = FindObjectOfType<playerController>();
        xdistance = transform.position.x - thePlayer.gameObject.transform.position.x;
        ydistance = transform.position.y - thePlayer.gameObject.transform.position.y;
    }

    // Update is called once per frame
    void Update()
    {

        distanceToMove = thePlayer.transform.position.x - lastPlayerPosition.x;

        distanceToMove = thePlayer.transform.position.y - lastPlayerPosition.y;

        transform.position = new Vector3(thePlayer.transform.position.x + xdistance, transform.position.y, transform.position.z);

        transform.position = new Vector3(transform.position.x, thePlayer.transform.position.y + ydistance, transform.position.z);

        lastPlayerPosition = thePlayer.transform.position;

    }*/
